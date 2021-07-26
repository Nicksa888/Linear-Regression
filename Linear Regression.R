#########################
#########################
#### Clear Workspace ####
#########################
#########################

rm(list = ls()) 
# clear global environment to remove all loaded data sets, functions and so on.

###################
###################
#### Libraries ####
###################
###################

library(easypackages) # enables the libraries function
suppressPackageStartupMessages(
  libraries("tidyverse",
            "stats",
            "corrplot",
            "olsrr", # enables normality of residuals check
            "car", # for durbinWatsonTest() for autocorrelation check
            "lmtest", # for coeftest()
            "QuantPsyc", # for lm.beta()
            "plyr",
            "lubridate" # for working with date information
            ))

setwd("C:/R Portfolio/Linear Regression/Data")
bikes <- read.csv("bikes.csv")

################
# Preview Data #
################

glimpse(bikes)
summary(bikes)
str(bikes)
dim(bikes)

# Convert categorical variables into factors

bikes$season <- as.factor(bikes$season)
bikes$holiday <- as.factor(bikes$holiday)
bikes$weekday <- as.factor(bikes$weekday)
bikes$weather <- as.factor(bikes$weather)

##########################
# Variable Relationships #
##########################

cov()

# Pearson Correlation Coefficients #

cor(bikes$humidity, bikes$rentals)
cor(bikes$windspeed, bikes$rentals)
cor(bikes$temperature, bikes$rentals)

############################
# Visualising Correlations #
############################

bikenumeric <- bikes %>%
  select(-date) # remove date variable as it is a character

bike_correlations <- cor(bikenumeric)

corrplot(bike_correlations)
corrplot(bike_correlations, type = "upper")
corrplot.mixed(bike_correlations) # see differences between correlations

##################################
##################################
# Simple Linear Regression Model #
##################################
##################################

bikes_modl <- lm(rentals ~ temperature, bikes)
bikes_modl
summary(bikes_modl)
# The Multiple R-squared value is 0.3937, tells us that temperature accounts for 39.37% of the variation in rentals. In other words, just over 60% of the variation is explained by other variables, which means other variables have an impact also.
# The adjusted r squared value informs how much variance is explained if the model had been derived from a population from which the sample was taken.
# The F-statistic is 473.5 and p-value is 2.2e-16, which means there is a less than 1% chance that an F-Ratio this large would occur if the null hypothesis is true, which is that temperature has no effect on rentals.
# 2.2e-16 means 2.2 with the decimal moved 16 places to the left, with 16 added zeros, so in effect it is 0.00000000000000022
# Therefore, the model results in a significantly better prediction of rentals than if we had used the mean value of rentals
# In terms of the coefficients, the intercept value means that when there are no increases in temperature, actual rentals falls by 167 
# The temperature coefficient means that as temperature increase by 1 degree, rental sales increase in number by 78.495

####################################
####################################
# Multiple Linear Regression Model #
####################################
####################################

bikes_modl2 <- lm(rentals ~ temperature + humidity + windspeed, bikes)
bikes_modl2
summary(bikes_modl2)

# The R2 figure is 0.4609, which according to Cohen's criteria is a medium effect as it is between .3 and .5. A large effect is + or minus .5, while a small effect is + or minus .1. Therefore, to ensure sufficient sample size, we need 80 observations as we have three predictors. As the data contains 731 rows, there is more than enough rows.
# The Multiple R-squared value is 0.4609, which as 0.5 is usually expected, it is a little low.
# The difference in r-squared values is 0.4609 - 0.4587, which is 0.0022. This means that if the model were derived from the population rather than the sample, the loss in explanation of variability is 0.0022%, so the model generalises very well. The cross validation of the model is very good.
# The F-statistic is 207.2 and p-value is 2.2e-16, which means there is a less than 1% chance that an F-Ratio this large would occur if the null hypothesis is true, which is that temperature has no effect on rentals.
# In terms of the coefficients, the intercept value means that when there are no increases in temperature, actual rentals increase by 2706.002 
# The temperature coefficient means that as temperature increase by 1 degree, rental sales increase in number by 78.316
# The humidity coefficient means that as humidity increase by 1 unit, rental sales decrease in number by -3100.123
# The windspeed coefficient means that as windspeed increase by 1 unit, rental sales decrease in number by -115.463
# The std. error is the standard deviation of the parameter estimates
# The residual is the observed value minus the predicted value. This means that , the number of bike rentals was overpredicted by the model by 4780.5 bikes, while as the maximum residual is 3653.5, which means that the model underpredicted by that many bikes
# The median residual value is -62.2, which means that at least half of the residuals are negative. This means that the predicted values are more than the observed in at least 50% of the cases.
# The residual standard error indicates that the actual bike rental number deviated from predictions on average by 1425

lm.beta(bikes_modl2)
# temperature    humidity   windspeed 
# 0.6260599  -0.2279295  -0.1923005 
# The standard deviation of the temperature parameter estimate is 3.464
# Therefore, for every 3.464 increase in Faranheit increase, an extra 2.168671 bikes are rented
0.6260599 * 3.464

plot(bikes_modl2)

####################
# Updating a Model #
####################

# Rather than writing out a new model with more predictors in, the update () function can be used

# bikes_modl3 <- update(bikes_modl2, .~. + season + holiday + weekday + weather + realfeel)

###############
###############
# Assumptions #
###############
###############

# The purpose here is to check suitability of the model to explaining the data

# The assumptions include that the residuals:

# must have a mean of zero
# are normally distributed
# have equal variance across the values of the independent variable (homoscedasticity)
# Are not correlated

##########################
# zero mean of residuals #
##########################

mean(bikes_modl2$residuals)
# The mean output is very close to zero and therefore, the assumption is met

##########################
# Normality of residuals #
##########################

ols_plot_resid_hist(bikes_modl2)

# The plot indicates the distribution is normal

hist(residuals(bikes_modl2),   # data are the residuals
     xlab = "Value of residual",      # x-axis label
     main = "",                       # no title 
     breaks = 20                      # lots of breaks
)

plot(bikes_modl2, which = 2) # indicates normality
shapiro.test(bikes_modl2)

##########################
# Linearity of residuals #
##########################

yhat <- fitted.values(bikes_modl2)
ggplot(bikes, aes(x = yhat, y = rentals)) +
  geom_point() +
  geom_smooth(method = "lm")

yhat <- fitted.values(bikes_modl2)
plot(x = yhat, 
     y = bikes$rentals,
     xlab = "Fitted Values",
     ylab = "Observed Values" 
)

plot(bikes_modl2, which = 1)

# there is some curvature, but perhaps not enough to be concerned about violation of this assumption

# Determine Linearity for each predictor

residualPlots(bikes_modl2) 

# Windspeed is perfectly linear, but the variables are not

#################################
# Homoscedasticity of residuals #
#################################

ols_plot_resid_fit(bikes_modl2)

# There is some evidence of homoscedasticity, as the residuals do not always form a horizontal band around the horizontal line. This can be resolved by using a weighted regression or apply a concave functio, for example a log transformation.

ncvTest(bikes_modl2) # non-constant variance test
# with a significance of 8.6226e-06, Homoscedasticity of residuals assumption has been violated  
# This means that regression coefficients are no longer entirely reliable, and so your  t-tests for the coefficients aren't quite right either

# To fix:

coeftest(bikes_modl2, vcov = hccm)

############################
# Residual Autocorrelation #
############################

durbinWatsonTest(bikes_modl2)

# The p-value of zero and and d-w test statistic of 0.4042771 indicates there is strong evidence of positive correlation of residuals.

# values between 0 and 2 indicate positive autocorrelation, while 2 indicates zero autocorrelation. Values between 2 and 4 indicate negative autocorrelation.
# Where autocorrelation is evident, it means the noise in the model is not down to random chance and there is more information needed to improve the model

############
# Outliers #
############

cooks.distance(bikes_modl2)
# A value of 1 or above is a cause of concern
?ols_plot_cooksd_chart
ols_plot_cooksd_chart(bikes_modl2)
str(ols_plot_cooksd_chart(bikes_modl2))
cooks_outliers <- as.numeric(unlist(ols_plot_cooksd_chart(bikes_modl2)$outliers[, "observations"]))
cooks_outliers
plot(bikes_modl2, which = 4) # indicates outliers based on Cooks distance
# There are three observations that are outliers, namely rows numbered 69, 204 and 239
# observation number 69 is significantly higher than the others, so let's explore it

bikes[69, c("rentals", "humidity", "windspeed", "temperature")]
summary(bikes[-69, c("rentals", "humidity", "windspeed", "temperature")])
# observation number 69 contains a zero for humidity, so, it can be classed as an outlier. Such an assertion is supported by the fact that when the data is explored without row number 69, the minimum humidity is 01879

outlier_index <- as.numeric(unlist(cooks_outliers[, "observations"]))
bikes2 <- bikes[-outlier_index, ]  # remove outliers from data

plot(bikes_modl2, which = 5)

bikes_modl3 <- lm(rentals ~ temperature + humidity + windspeed, bikes, 
   subset = c(-69, -204, -239)           # ...but observation 69, 204 and 239 are deleted
)
bikes_modl3

#####################
# MultiCollinearity #
#####################

# check variance inflation factors
# a vif of greater than five or a tolerance of less than 0.2 indicates prescence of mulitcollinearity

vif(bikes_modl2)
ols_vif_tol(bikes_modl2)

###################
###################
# Model Selection #
###################
###################

full.model <- lm(rentals ~ season + holiday + weekday + weather+ temperature + humidity + windspeed, bikes)
step(object = full.model,     # start at the full model
     direction = "backward"   # allow it remove predictors but not add them
)

null.model <- lm(rentals ~ 1, bikes)   # intercept only.
step( object = null.model,     # start with null.model
      direction = "forward",   # only consider "addition" moves
      scope =  rentals ~ season + holiday + weekday + weather+ temperature + humidity + windspeed)

######################
######################
# Compare Two Models #
######################
######################

M0 <- lm(rentals ~ season + holiday + weekday + weather+ temperature + humidity + windspeed, bikes)
M1 <- lm(rentals ~ holiday + weekday + weather+ temperature + humidity + windspeed, bikes)
AIC(M0, M1)
# M0 has the smaller AIC value, so is a better fit

anova(M0, M1)

#######################
#######################
# Improving the Model #
#######################
#######################

# temperature, humidity look like quadratic as there is a u shaped curve
# We add squared versions of variables to the model

bikes2 <- bikes %>%
  mutate(humidity2 = humidity^2) %>%
  mutate(temperature2 = temperature^2)

bikes_mod3 <- 
  lm(rentals ~ temperature + humidity + windspeed + humidity2 + temperature2, bikes2)
summary(bikes_mod3)

##################################
##################################
# Consider Categorical Variables #
##################################
##################################

summary(bikes2[, c("season", "holiday", "weekday", "weather")])

library(plyr)
bikes2$season <- mapvalues(bikes2$season, from = c("1", "2", "3", "4"), to = c("Winter", "Spring", "Summer", "Fall"))
bikes2$holiday <- mapvalues(bikes2$holiday, from = c("0", "1"), to = c("No", "Yes"))
bikes2$weekday <- mapvalues(bikes2$weekday, from = c("0", "1", "2", "3", "4", "5", "6"), to = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
bikes2$weather <- mapvalues(bikes2$weather, from = c("1", "2", "3"), 
to = c("Clear", "Light Precipitation", "Heavy Precipitation"))

bikes_mod4 <- 
  lm(rentals ~ temperature + humidity + windspeed + humidity2 + temperature2 + season + holiday + weekday + weather, bikes2)
summary(bikes_mod4)

# The spring coefficient means that compared to winter (as the baseline), an extra 590 bikes were rented.
# The multiple r-squared value means that 62.56 of the variation has been explained.

##############################################
##############################################
# Considering interactions between variables #
##############################################
##############################################

# There are times when two variables have a combined effect on the response variables
# In this example, weather and windspeed could have some sort of interaction

bikes_mod5 <- 
  lm(rentals ~ temperature + humidity +  humidity2 + temperature2 + season + holiday + weekday + weather * windspeed, bikes2)
summary(bikes_mod5)

# The multiple r-squared number has increased from model number 4 to 63.23%, so more variance is explained, so it is an improved model.
# As weather heavy and the interaction between weather heavy and windspeed is negative that means for every 10mph increase in windspeed (the units of the windspeed variable), when there is heavy precipitation, bike rentals are decreased by the following formula:
# (-1095.9415 * 10 - 23.3974 * 10)


