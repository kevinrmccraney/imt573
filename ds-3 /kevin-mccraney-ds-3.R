#
# Kevin McCraney
# DS Homework 3
# 02/23/2018
#
#
#
setwd("/Users/kmccraney/Desktop/ds-3")
#
#
# QUESTION 1
#
# a. In a survey, one hundred college students are asked how many hours per week they
# spend on the Internet.
#
# The paramater of interest here is a mean. Sum the number of hours and divide by the population.
#
#
#
#
# b. In a survey, one hundred college students are asked: “What percentage of the time
# you spend on the Internet is part of your course work?”
#
# The parameter of interest here is a mean. While the item measured is a proportion (i.e. what part of
# the whole do you spend on schoolwork), we're interested in the average of all these percentages.
#
#
#
#
# c. In a survey, one hundred college students are asked whether or not they cited
# information from Wikipedia in their papers.
#
# The parameter of interest here is a proportion. This is a categorical variable (i.e. cited/not cited).
#
#
#
# d. In a survey, one hundred college students are asked what percentage of their
# total weekly spending is on alcoholic beverages.
#
# The parameter of interest here is a mean. Since the percentage weight is already a proportion
# we're ostentibly looking for the average amount of weekly spending across the population.
#
#
#
# e. In a sample of one hundred recent college graduates, it is found that 85 percent expect
# to get a job within one year of their graduation date.
#
#
# The parameter of interest here is a proportion. 85% is part of a whole, and in order to get to that
# measure, we'd probably use a categorical variable (i.e. do you expect to get a job after graduation,
# yes or no?) and gauge the parts of the whole that responded.
#
#
#
# QUESTION 2
#
# In 2013, the Pew Research Foundation reported that “45% of U.S. adults report that they live with
# one or more chronic conditions”. However, this value was based on a sample, so it may not be a perfect
# estimate for the population parameter of interest on its own. The study reported a standard error of
# about 1.2%, and a normal model may reasonably be used in this setting. Create a 95% confidence interval
# for the proportion of U.S. adults who live with one or more chronic conditions. Also interpret the
# confidence interval in the context of the study.
#
SE <- .012
Zscore <- 1.96
PointEstimate <- .45
#
negCI <- PointEstimate - (SE*Zscore)
posCI <- PointEstimate + (SE*Zscore)
#
# 95% CI = point estimate ± Z * SE
# 95% CI = 0.45 ± 1.96 * 0.012
# 95% CI = 0.45 ± 0.02352
#
# At the 95% CI, between 42.648% and 47.352% of adults may live with 1+ chronic illnesses in the US.
# A confidence interval is defined as a range of values that--with some certainty--contains the parameter
# of interest. That means 95% of the time we should expect to see these results we saw, and our point
# estimate falls within this range, which is good!
#
#
#
#
#
# QUESTION 3
#
# The nutrition label on a bag of potato chips says that a one ounce (28 gram) serving of potato chips
# has 130 calories and contains ten grams of fat, with three grams of saturated fat. A random sample of
# 35 bags yielded a sample mean of 136 calories with a standard deviation of 17 calories.
#
# a. Write down the null and alternative hypotheses for a two-sided test of whether
# the nutrition label is lying.
#
# Null Hypothesis: 1 serving (i.e. 28g) of chips = 130cal
# Alternative hypothesis: 1 serving (i.e. 28g) of chips ≠ 130cal
#
# We use the indicator ≠ to imply it's a 2-sided test; using > or < would suggest one tail of the
# distribution in particular.
#
#
#
#
#
#
# b. Calculate the test statistic and find the p value.
#
# test statistic = t = (xbar - mu) / (std dev / sqrt(sample size))
xbar <- 136
mu <- 130
stdDev <- 17
sampleSize <- 35
t <- (xbar - mu) / (stdDev / sqrt(sampleSize)) # test statistic = 2.088
#
#
# To find the p value given the test statistic, we can use the pnorm function:
potatoPvalue <- 1-pnorm(t)
#
# That gives us a p-value of 0.0183 for the given test statistic.
#
#
#
#
#
# c. If you were the potato chip company would you rather have your alpha = 0.05 or 0.025 in this case?
# Why?
#
# The p-value determines whether the sample provides the requisite evidence to reject the null hypothesis.
#
# Since we are trying to disprove the null hypothesis, with a p-value of 0.025
# we would theoretically see an observation like this one ~2.5% of the time.
# As the company, we want to create the smallest possible opportunities to misinform our stakeholders.
# With a p-value of 0.025, our trials could more readily prevent incorrect rejections of the null, and thus
# fewer opportunities to suggest that a serving does not contain the amount we say it does.
#
# Overall, the answer depends on whether this is an internal trial or if it's conducted outside the company.
# I'd argue that having a p-value of 0.05 gives a company more plausible deniability (say, if external
# trials were being conducted on its products), but that's a more adverserial perspective for chip makers.
#
#
# QUESTION 4
#
# Regression was originally used by Francis Galton to study the relationship between parents and children.
# He wondered if he could predict a man’s height based on the height of his father? This is the question we
# will explore in this problem. You can obtain data similar to that used by Galton as follows:
#
library(UsingR)
height <- get("father.son")
#
# a. What does the relationship look like? Would a linear model be appropriate here?
#
# 
# plot as points
plot(height,
     main="Father's Height vs. Son's Height",
     xlab="Father's Height (in)",
     ylab="Son's Height (in)",
     type="p")
#
# If we plot this as a series of points, it's pretty evident that there's a relationship between the 
# height of a father and the height of a son in aggregate; as the father's height goes up, it looks like
# a son's height also goes up.
#
# Within our conceptual "cloud of points" model for determining whether or not we can adequately use
# a linear model to fit data (i.e. there are few wild outliers), we've hit the points we need to.
# Thus we can use a linear model. But let's look to correlation to figure things out further...
#
#
cor(height)
#
# Indeed, we see that there's a ~0.5 positive correlation between the two. Good sign.
#
# b. Fit a linear model, filling in coefficient values and interpreting coefficient estimates. 
#
model <- lm(sheight~fheight, data=height)
#
# ysheight = β0 + β1 x fheight
#
# Using the equation form here, we obtain:
# son's height = 33.8866 + 0.5141 * father's height
#
# This means that for every 1in increase in a father's height,
# we should see 0.5141in increase in the son's.
# If we initialize the father's height at 0, the son's height is 33.8866.
#
#
#
# c. Find the 95% confidence intervals for the estimates.
#
confint(model, level = 0.95)
#
#                  2.5 %     97.5 %
# (Intercept) 30.2912126 37.4819961
# fheight      0.4610188  0.5671673
#
# Using the confint() function, we see the resultant confidence intervals from above.
# Intercept 95%CI               (30.2912126, 37.4819961)
# father's height(slope) 95%CI  (0.4610188,  0.5671673)
#
#
# d. Produce a visualization of the data and the least squares regression line.
#
# Since we already addressed this above (sorta), let's reuse the graph we had before and
# superimpose a red regression line.
#
plot(height,
     main="Father's Height vs. Son's Height",
     xlab="Father's Height (in)",
     ylab="Son's Height (in)",
     type="p")
#
abline(model, col="red")
#
# It looks like the red line has a positive upward trend.
#
# e. Produce a visualization of the residuals versus the fitted values. 
# (You can inspect the elements of the linear model object in R using names()). 
# Discuss what you see. Do you have any concerns about the linear model?
#
#
hist(model$residuals,
     main="Residuals of Father/Son Linear Model",
     xlab="Model Residuals")
#
# Visualizing the model's residuals allows us to see that they are fairly normally distributed, which means
# that a linear model would be accurate in this set of circumstances. With all the clustering around 0,
# it seems like things are pretty accurate.
#
#
# f. Using the model, predict height of sons for dads that are 50, 55, 70, 75, and 90 inches respectively.
#
dads <- data.frame(fheight = c(50, 55, 70, 75, 90))
predict(model, newdata=dads)
#
# The model predicts...
# ...that a father of 50in will have a son of 59.59in.
# ...that a father of 55in will have a son of 62.16in.
# ...that a father of 70in will have a son of 69.87in.
# ...that a father of 75in will have a son of 72.44in.
# ...that a father of 90in will have a son of 80.15in.
#
#
# g. What do the estimates of the slope and height mean?
# Are the results statistically significant? Are they practically significant?
#
summary(model)
# The estimate of the slope suggests that for every 1in change in a father's height, the son's height
# will increase by 0.51in. The intercept of 33.89 suggests that if we initiate a father's height at 0,
# a son's height will be 33.89in.
#
#
# The results are statistically significant, but this doesn't practically mean anything.
# Because the change in height is so small overall, we see a genetic "regression toward the mean"
# occurring (i.e. tall fathers have shorter sons, and short fathers have taller sons).
# Humans have been around for a long time. Though over several generations height might increase,
# if this process didn't occur we'd have really enormous
# people by now, and would probably have depleted the world's resources.
#
#
# QUESTION 5
#
#
#
# An investigator is interested in understanding the relationship, if any, between the analytical skills of
# young gifted children and the father’s IQ, the mother’s IQ, and hours of educational TV.
#
# Let's load the data.
###### QUESTION: in terms of style, do we load libraries inline in R (i.e. when they are called)
###### or at the top of the script?
#
library(openintro)
data(gifted)
#
# a. Run two regressions: one with the child’s analytical skills test score (“score”) and the
# father’s IQ (“fatheriq”) and the child’s score and the mother’s IQ score (“motheriq”).
#
moddad <- lm(score~fatheriq, data=gifted)
modmom <- lm(score~motheriq, data=gifted)
moddad
modmom
#
plot(gifted$fatheriq, gifted$score, main="Father's IQ vs. Child's IQ",
     xlab="Father's IQ (pts)", ylab="Child's IQ (pts)")
abline(moddad, col='red')
#
plot(gifted$motheriq, gifted$score, main="Mother's IQ vs. Child's IQ",
     xlab="Mother's IQ (pts)", ylab="Child's IQ (pts)")
abline(modmom, col='red')
#
#
# b. What are the estimates of the slopes for father and mother’s IQ score with their 95% confidence
# intervals? (Note, estimates and confidence intervals are usually reported:
# Estimate (95% CI: CIlower, CIupper)
#
# 
confint(moddad, level = 0.95)
# For fathers: Slope (95% CI: -0.205, 0.705)
#               Intercept (95% CI: 78.155, 182.704)
#
#
#
confint(modmom, level = 0.95)
# For mothers: Slope (95% CI: 0.203, 0.610)
#               Intercept (95% CI: 86.997, 135.189)
#
#
# c. How are these interpreted?
#
# Practically, we interpret these data to mean that for 95% of observations, we would expect a child's
# IQ to fall within the interval of 86.99 and 135 (based on the mother model). We would also expect that for
# every 1 unit increase of a mother's IQ, that a child's IQ would increase by some number within the interval
# of 0.203 and 0.610.
#
#
# Similarly, (again, for 95% of observations) we would expect a child's IQ to fall within a
# range of 78.16 and 182.70 (for the father model).
# For 1 unit increase of a father's IQ, a child's IQ would increase by some number within the interval
# of -0.205 and 0.705.
#
#
#
#
# d. What conclusions can you draw about the association between the child’s score
# and the mother and father’s IQ?
#
# Broadly, based on the models, we see that for 1 unit of mom's IQ, the child's IQ goes up by 0.4 points.
# For fathers, it's lower at 1 father point for 0.25 child points. If we initialize a mom's IQ at 0,
# the child's IQ is 111.093; for a dad at 0, it's 130.429.
#
# It seems that the mother's IQ is a stronger positive predictor for a child's IQ than the father's.
# If the mother has a high score, then one can assume that the child will also have a high score with
# a (somewhat) narrow positive confidence interval.
#
# The father's IQ introduces much more variance but also suggests a positive trend upward, despite a wide CI.
#
# I would anticipate that the wider CI we see related to the father's score has something to do
# with the y-chromosome being 'broken' and degenerating over generations in a way the x doesn't
# (causing all kinds of unpredictability in terms of heritance), but that's only a guess.
#
# Ultimately, it seems like there's more variance in male IQ scores (pretty dramatic variance), and
# relative stability in female IQ scores. In a weird thought experiment, if two mothers could
# produce a child, they would have a more consistently high IQ score, and if 2 fathers had a child
# it would be harder to anticipate the child's IQ score. This is, of course, based on a small population
# so the possible inference from this is limited.