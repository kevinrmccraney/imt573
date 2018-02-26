# Kevin McCraney
#
# In-class lab #2
#
getwd()
setwd("/Users/kmccraney/Desktop/lab2")
# load the data
#
movies <- read.csv("movie.titles.csv")
ratings <- read.csv("ratings.csv")
#
#
summary(ratings)
summary(movies)
# 1a. There are 671 raters, with 163949 movies rated in total (this is aggregate, not unique).
# b. The max rating is 5, the mean is 3.544, and the lowest rating is 0.5.
# Further, median is 4, the standard deviation is ~1.06.
hist(rating$ratings)
# c. People definitely prefer round numbers for ratings.
#
temp <- merge(ratings, movies, by="movieId")
#
plot(temp$rating ~ temp$year.x)
# If you plot this as a histogram it's near-impossible to learn anything
# from this information because of the information density.
# Let's try a boxplot.
# 
boxplot(rating~year.x, data=temp, main="Rating by Year")
#
# With a box plot, more information is foregrounded.
# But it's still hard to see, so let's look at it with cut().
#
boxplot(rating~cut(year.x, 25), data=temp, main="Rating by Year")
# It seems that there is more variance over time in the overall
# ratings of movies (especially from 1957-1980),
# and that the ratings have trended downward.
# That is, ratings have become more harsh for films in the recent decades.
#
#
# 4. Exploring relationships
ratings$comedy <- rep(F, nrow=ratings)
ratings$comedy[grep("comedy",ratings$genre, ignore.case = T)] <- T
#
# let's look at horror
ratings$horror <- rep(F, nrow=ratings)
ratings$horror[grep("horror",ratings$genre, ignore.case = T)] <- T
#
# let's look at action
ratings$action <- rep(F, nrow=ratings)
ratings$action[grep("action",ratings$genre, ignore.case = T)] <- T
#
# plot comedy vs horror vs action vs all
boxplot(ratings$rating[ratings$comedy], ratings$rating[ratings$horror], ratings$rating[ratings$action], ratings$rating[!ratings$comedy])
t.test(ratings$rating[ratings$comedy], ratings$rating[!ratings$comedy])
#
#
#
# 5. make your own metric -- didn't get to this one! sorry!
#
#
#
#
# PART 2:
# 1. Plotting different distributions
#
# exponential:
exponential <- rexp(1000)
hist(exponential)
#
# uniform:
uniform <- runif(1000)
hist(uniform)
#
# and poisson,
# can't resist a good pun...
fish <- rpois(1000, .5)
hist(fish)
#
#
#
#
# 2. And now looking at the distributions of the sum of these samples:
#
#
# using some copy-pasted code:
#
N <- 1000  # number of exponential draws
n.samp <- 30 # number of sums to take
M <- matrix(NA, nrow=N, ncol=n.samp) # create empty matrix to fill with samples
for(j in 1:n.samp) M[,j] <- rexp(N) #generate the samples
hist(rowSums(M), freq = F) # plot a histogram of sums across rows of our matrix M
#
# and then combined with the charted curve:
curve(dnorm(x, mean(rowSums(M)), sd(rowSums(M))), min(rowSums(M)), max(rowSums(M)), add=T, col="red", lwd=2)
#
#
# 3.
# ...interesting: this is for the uniform distribution
#
M <- matrix(NA, nrow=N, ncol=n.samp) # create empty matrix to fill with samples
for(j in 1:n.samp) M[,j] <- runif(N) #generate the samples
hist(rowSums(M), freq = F) # plot a histogram of sums across rows of our matrix M
curve(dnorm(x, mean(rowSums(M)), sd(rowSums(M))), min(rowSums(M)), max(rowSums(M)), add=T, col="red", lwd=2)
#
# ...and for poisson
#
M <- matrix(NA, nrow=N, ncol=n.samp) # create empty matrix to fill with samples
for(j in 1:n.samp) M[,j] <- rpois(1000, .5) #generate the samples
hist(rowSums(M), freq = F) # plot a histogram of sums across rows of our matrix M
curve(dnorm(x, mean(rowSums(M)), sd(rowSums(M))), min(rowSums(M)), max(rowSums(M)), add=T, col="red", lwd=2)
#
#
# 4. As you increase the number of samples within the distribution, it
# converges toward the expected model for the system
# This is (as Ben put when we talked about it) the law of large numbers.
# If you change the number of sums taken, the resolution of the histogram will
# increase/decrease and it will more resemble the Platonic normal distribution
# or look less like it (respectively).