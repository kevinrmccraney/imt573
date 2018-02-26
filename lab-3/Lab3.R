# Kevin McCraney
# Lab 3
# 02/07/2018
#
#
#
# 0. Plotting the data
setwd("/Users/kmccraney/Desktop/Lab_3_export")
load("BostonData.rdat")
plot(Boston)
#
# It seems like there is a pretty consistent relationship between NO concentration and distance to work,
# and (within a certain distance) home.value and distance to work. 
# Otherwise I don't see much of a relationship. I am really tired right now though, and my critical
# faculties may not be effective.
#
# 1. Fitting single linear regressions
# Home value to NO concetration
mod1 <- lm(home.value ~ NO.concentration, data = boston)
summary(mod1)
plot(fitted(mod1), resid(mod1))
# It seems that there is not a very significant relationship between these variables.
#
#
#
mod2 <- lm(home.value ~ distance.to.work, data = boston)
summary(mod2)
# It seems that there is a very significant linear relationship between these two variables.
#
mod3 <- lm(home.value ~ student.teacher.ratio, data = boston)
summary(mod3)
#
# It seems that there is a somewhat significant relationship between these two variables.
#
#
# 2. Comparing the r-squared values for the three linear regressions
#
# Multiple R-squared:  0.1826,	Adjusted R-squared:  0.181 
# Multiple R-squared:  0.06246,	Adjusted R-squared:  0.0606 
# Multiple R-squared:  0.2578,	Adjusted R-squared:  0.2564 
#
# It seems that student teacher ratio is best for this model (but it could be overfit).
#
#
#
# 3. Creating a multiple linear regression model
#
mod.full <- lm(home.value ~ distance.to.work + NO.concentration + student.teacher.ratio, data = boston)
summary(mod.full)
#
# When you create a multi linear model, it seems that distance to work is still the best predictor,
# but it switches signs for some weird reason.
# Perhaps it's because it doesn't make sense to fit a linear value here--as the distance your house is
# from work goes to infinity, the price doesn't go to zero as well.
#
#
#
# 4. Which model fits the data best?
#
# SINGLE LINEAR MODELS
# Multiple R-squared:  0.1826,	Adjusted R-squared:  0.181 
# Multiple R-squared:  0.06246,	Adjusted R-squared:  0.0606 
# Multiple R-squared:  0.2578,	Adjusted R-squared:  0.2564 
#
#
# MULTI LINEAR MODEL
# Multiple R-squared:  0.4061,	Adjusted R-squared:  0.4026 
#
# It seems that the multi-linear model fits the data best, but that does not necessarily mean that it
# will accomodate new data the best.
#
#
#
# 5. Predict and find prediction interval
predict(mod.full, newdata=data.frame("distance.to.work" = 3, "NO.concentration" = 0.35, "student.teacher.ratio" = 10), interval="prediction")
#
# The prediction is ~49.57k for the home, with a prediction interval between 35.2k and 63.92k.
#