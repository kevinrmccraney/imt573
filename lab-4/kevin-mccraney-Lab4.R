# Kevin McCraney
# Lab 4
# 02/21/2018
#
#
setwd("/Users/kmccraney/Desktop/ML-Lab")
data <- read.csv("census data.csv")
#
#
# 1. Make a new column representing >50k as a categorical variable.
data$income.g50 <- rep(0, nrow(data))
data$income.g50[data$income==" >50K"] <- 1
#
#
# 2. 
mod <- glm(income.g50 ~ education + age + sex + race, 
           data=data[,!colnames(data)%in%"income"], family="binomial")
#
summary(mod)
#
# a. The log odds ratio for having a masters degree is 2.91 (statistically significant),
# while a 1st-4th grade education is -0.18 (not so). That is, compared to a 10th grade education, having
# a masters is...
exp(2.91)
# 18x greater. With my understanding of the way that work works in the modern era, it makes sense
# that there would be such a dramatic difference between the earning potential of a high-school dropout
# and someone who gets a master's.
#
exp(-0.47) < exp(-0.18)
#
# Curiously this is not the case for 1st-4th grade; it is actually more detrimental to have a 9th grade
# education. Where I previously attributed the change to the way the world works, I'd attribute this difference
# to the way our modern educational system works. If you get out early, or complete it on time, it won't
# negatively affect your earning potential as much, perhaps?
# 
# If we are taking into account multiple comparisons (the idea that the more inferences we make, the more
# likely we are to produce error), it seems like the effects would compound greatly. We would have to be more
# strict for our significance threshold. We should probably pick the smallest of the small p-values then.
#
# b. The log odds ratio for being male increases by 1.29 (statistically significant), and age by 0.04
# (also statistically significant, to an equivalent degree). That means that being male increases "success"
# by almost fourfold. They are both definitely statistically significant, as the p-value is incredibly low.
#
# They are also practically significant. Men (especially men of a certain race) are given many more
# opportunities in education and in life, so there are compounding effects related to higher levels of
# education and wealth and so on.
#
# This is probably not fair, but part of that depends on your worldview. Men often take more risks in their
# career and with risk comes more opportunity for financial rewards, but at the same time women do a lot of
# unpaid and emotional labor, so they are working the same amount (or greater) for lesser rewards. Complicated.
#
# Maybe for age it's fair. If you get older, you have more experience, and might get paid better?
#
#
# 3. Exploring Relationships II
x <- data$age
plot(x, data$income.g50, col="blue")
fits <- fitted(mod)
points(x, fits, pch=19, cex=0.3)
#
# The predicted probabilities are as variable as they are because age, when coupled with education, race, and so
# on, is somewhat deterministic. Below a certain age it's not possible to have obtained a certain
# level of education, which means that your income will be weighted toward the lower quartile of the prediction.
# The graph opens up around the 30's and 40's more, because there's a stronger case that people will be 'fixed'
# in their education level by then. Then the model can more readily predict outcomes.
#
# 4. Exploring probability cutoffs
#
#
tab25 <- table(data$income.g50, fits>=0.25)
(tab25[1,2]+tab25[2,1])/sum(tab25)
#
tab50 <- table(data$income.g50, fits>=0.5)
(tab50[1,2]+tab50[2,1])/sum(tab50)
#
tab75 <- table(data$income.g50, fits>=0.75)
(tab75[1,2]+tab75[2,1])/sum(tab75)
#
# The lowest percent error is if we tabulate the data at 50%.
# The percent error corresponds to 20.6% error.
#
#
# 5. Examine the model
# a. plot the curve
#
library(AUC)
y <- factor(data$income.g50)
rr <- roc(fits, y)
plot(rr)
auc(rr)
#
# b. and gauge how well it fits
#
# After calculating the AUC, it seems like the model accurately represents 80.2% of the data.
# I'd say that's pretty good--it's better than chance, and doesn't seem like overfit--but there's
# probably room for improvement in the realm of sensitivity. So let's...
#
#
# 6. Formulate another model.
#
mod2 <- glm(income.g50~.,
           data=data[,!colnames(data)%in%c("income")], family="binomial")
#
summary(mod2)
#
# Interestingly, the odds have changed (in some cases dramatically, 
# and in some cases slightly), but the same categories that were previously 
# significant have retained their significance, and actually gone up in terms of their significance.
#
#
plot(x, data$income.g50, col="blue")
fits2 <- fitted(mod2)
points(x, fits2, pch=19, cex=0.3)
#
#
# b.
# The predictions do not follow the same kind of trajectory as the other model. As we introduce
# more variables we introduce more potential for prediction (and for error).
# Since we're looking at a number of different factors, this almost takes the shape of
# a normal distribution, as opposed to a logit.
#
#
#
# c.
alttab25 <- table(data$income.g50, fits2>=0.25)
(alttab25[1,2]+alttab25[2,1])/sum(alttab25)
#
alttab50 <- table(data$income.g50, fits2>=0.5)
(alttab50[1,2]+alttab50[2,1])/sum(alttab50)
#
alttab75 <- table(data$income.g50, fits2>=0.75)
(alttab75[1,2]+alttab75[2,1])/sum(alttab75)
#
# We see the lowest error at 50% as well, with error of 16.6%. This is the lowest between the two
# models, so it seems that this one performs better.
#
#
# d.
y <- factor(data$income.g50)
rr2 <- roc(fits2, y)
plot(rr2)
auc(rr2)
#
# After plotting the ROC and calculating the AOC again, it looks like this outperforms the
# other model by about 8%. That's a 10% increase in quality, which is pretty nice. In order to test,
# the effectiveness, however, we'd have to do k-fold validation and cross-test sections of the data
# against one another to gauge.
#
#
#
#
#
#
# Extra Credit:
#
# Trying out k-fold cross validation and interpreting the results:
#
#
# I found this really good library and read the docs for it a bit. We'll see if I do it right...
#
# You should probably run install.packages("caret") if you don't have it
#
library(caret)
set.seed(23434)
#
#
# This is the part where we build the model, based on the syntax for the package.
# We use the same form as the previous models in vanilla r.
# Here's 1:
#
kval1 <- train(income.g50 ~ education + age + sex + race, 
           data=data[,!colnames(data)%in%"income"],
           method="lm",
           tuneLength=9,
           trControl= trainControl(
             method="cv", number=10,
             verboseIter = TRUE
           ))
#
# And here's 2:
#
kval2 <- train(income.g50~., 
               data=data[,!colnames(data)%in%"income"],
               method="lm",
               tuneLength=9,
               trControl= trainControl(
                 method="cv", number=10,
                 verboseIter = TRUE
               ))
#
# So to unpack this, they're working with the same data, and breaking it into 10 parts.
#
kval1
kval2
#
#
# The r-squared statistic allows us to understand the variance as explained by the 
# model--that is, how much variance is explained by the model. Higher is better.
#
# Root-mean-squared error is the distance of the residuals from the line that the model
# fits to--it extends from the r-squared statistic, in that it takes into account the data
# that is not explained by the model. Lower is better, but it's not a catch-all.
#
# Mean absolute error accounts for large residuals by taking the mean of the absolute value of
# them. That allows us to understand how well our model does with outliers. Again, lower is
# better, but it's not a catch-all.
#
#
# These are the results for model 1:
# RMSE       Rsquared   MAE      
# 0.3831269  0.2077977  0.3040524
#
# ...and for model 2:
#   RMSE       Rsquared   MAE      
# 0.3504545  0.3376436  0.2730641
#
# Interpreting the results of these data, it seems that the second model explains our data better.
# The r-squared value is much higher, and the error values are (slightly) lower.
#