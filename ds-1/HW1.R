# Kevin McCraney
# DS First Assignment
getwd()
setwd("/Users/kmccraney/Desktop/ds-1")
#
#
# ANSWERS TO QUESTION 1
#
# 1a. Being an independent and being a swing voter are not disjoint. 
# We see this because the probability of the two of them occurring
# at the same time is nonzero (i.e. 11%).
#
# b. From the information given, we know that 35% of respondents 
# are independent but not swing.
#
# c. P(I) = .35    P(S) = .23    P(B) = .11    P(I∩B) = .24    P(S∩B) = .12
# P(IoS) = P(S∩B) + P(I∩B) + P(B). = .24 + .12 + .11 = 0.47
#
# d. We have to find the complement of P(IoS) to answer this question. 
# 1-P(IoS) = 1-.47 = 0.53
#
# e. Dependence implies that the events have to happen in some particular order,
# because the outcome of one event influences another. If events are independent,
# then P(A) * P(B) = P(AB). This is not the case here,
# because .35 * .23 = .08, not .11 . Thus, the events are dependent.
#
#
#
#
#
# ANSWERS TO QUESTION 2
#
# load felix data (& randy data for later)
hz <- read.csv("FelixHernandez2015.csv")
rj <- read.csv("RandyJohnson1995.csv")

# 2a. This variable contains the number of wins this year, which is 18.
sumWins <- sum(hz[,"W"])

# 2b. mean, median, mode of strikeouts in 2015
# but first, let's write a function for mode

strikeouts <- hz[,"SO"]

mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# mean = 6.16
# median = 6
# mode = 5

mean(strikeouts)
median(strikeouts)
mode(strikeouts)

# 2c. plot the relationships between innings pitched/strikeouts &
# innings pitched/walks.

innings <- hz[,"IP"]
balls <- hz[,"BB"]

plot(innings, strikeouts, col="red")
par(new=TRUE)
plot(innings, balls, col="green")

# In terms of what the plots look like, there is a bit of a relationship. 
# Later on in a game there tends to be more strikeouts and balls.
# Perhaps a batter's visual accuity gets worse later in a game,
# or they just get tired and worse at playing? Conversely, maybe a pitcher
# gets better as a game goes on and they have time to warm up their arm?
# (I don't know much about baseball so these are naive guesses...)
#
#
#
cor(strikeouts/innings, balls/innings, method="spearman")
# 2d. Calculate correlation between strikeouts & balls w/r/t innings
# There is a strong positive Pearson correlation coefficient of .98, which means
# there is a relationship of some kind between strikeouts/balls over time.
# This aligns with what I saw in the plot.
#
#
month <- hz[,"Month"]
meanMonth <- by(balls, month, mean)
varMonth <- by(balls, month, var)
plot(meanMonth, col="red")
par(new=TRUE)
plot(varMonth, col="green")
#
# 2e. Similar to 2d, the mean and variance in number of walks goes up throughout
# the season. My hypothesis is similar to the previous one, that there are fewer
# walks at the beginning of a season because pitchers are more precise in their
# throws as a season starts and then get worse as time goes on.
#
# The variance across games could be due to a team playing at home or visiting,
# the temperature, or a number of different factors, so I am less confident in
# coming up with a clear hypothesis there. Is there a "hot hands" equivalent for
# baseball? :P
#
#
# 2f. We see that when Felix is away he wins fewer games than when he is home.
# At home he won 11 games and away he only won 7.
away <- hz[,"away"]
wins <- hz[,"W"]
winsAway <- by(wins, away, sum)
winsAway
#
#
# 2g. Randy vs. Felix
# Randy outperforms Felix in terms of having a larger mean and sum.
# Based on that, Randy has a higher total number of strikeouts across the season,
# and more per game. I would probaby say Randy is a better player, but again,
# I don't really know baseball...
# 
#
rj.strikeouts <- rj[,"SO"]
sum(rj.strikeouts) > sum(strikeouts)
mean(rj.strikeouts) > mean(strikeouts)
#
#
#
#
#
# ANSWERS TO QUESTION 3
#
# 3a. Calculate Sophia's z-score
#
# sophia's information
sv <- 156
sq <- 157
# population information
pv <- 151
pq <- 153
# standard deviations
vsd <- 7
qsd <- 7.67
# calculating zscore
vz <- (sv - pv)/vsd
qz <- (sq - pq)/qsd
#
# 3b. plot a normal distribution and draw the z-scores on it 
#
# 
# First, I made a standard normal distribution with mean 0 and std dev 1.
#
x <- seq(-4,4,length=1000)
y <- dnorm(x,mean=0, sd=1)
#
# And then I plotted it, with some lines and text for clarity.
plot(x,y, type="l", lwd=2, main="Normal Distribution with Overlaid Z-Scores")
abline(v=vz, col="blue4")
text(vz+.5, .05, labels="verbal", col="blue4")
abline(v=qz, col="green4")
text(qz-.5, .3, labels="quant", col="green4")
#
#
#
# 3c. Relative to others, Sophia did better on Verbal.
pnorm(vz) > pnorm(qz)
#
# and you can do the same thing by using:
pnorm(sv, pv, vsd) > pnorm(sq, pq, qsd)
#
#
# 3d find percentile scores
# sophia scored in the 76th percentile for verbal
# and in the 70th percentile for quantitative
vper <- round(pnorm(vz)*100.0)
qper <- round(pnorm(qz)*100.0)
vper
qper
#
# 3e. Percent that did better than Sophia:
# 24% of people did better than her on verbal
# 30% of people did better than her on quant
verbalBetter <- 100-vper
quantBetter <- 100-qper
verbalBetter
quantBetter
#
# 3f. Why comparing raw scores doesn't make sense:
#
# Looking at the raw scores, Sophia scored one point better on
# the quantitative reasoning section. However, looking at the
# percentage scores, only 24% of test takers did better than her on
# the verbal section (compared to 30% in the quantitative section).
# She may have scored higher on the quantitative section, but more
# people got perfect scores there, thus skewing the distribution
# and making her high score not as important.
# We have to take into account the performance within
# the population overall, and that means looking at the distribution
# across both parts of the test. We use the z-score to
# measure similarity within 2 different distributions like these. 