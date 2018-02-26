#
# Kevin McCraney
# DS Homework 2
#
#
library(nycflights13) # load library
#
data(flights) # load data
#
# If we're just loading a library and loading data from that library, do we really
# have to setwd()?
setwd("/Users/kmccraney/Desktop/ds-2")
#
# let's see what we're working with...
head(flights)
#
# 1 - Flights from NYC to Seattle
# Note that NYC airports are JFK, LGA, EWR, so let's make a vector...
NYC <- c("JFK", "LGA", "EWR")
#
# a. How many flights were there to and from NYC in 2013?
# pseudocode for this: sum of length of vector select origin=NYC + select dest=NYC
flights[flights$origin %in% NYC, ]
#
# 
# There were 336,776 flight entries with an origin of NYC, which means there were
# that many individual events called "flights"...
flights[flights$dest %in% NYC, ]
#
#
#
# EVERYTHING ELSE HERE IS AN ASIDE...
#
# When I got this answer I thought i was being real dumb because it was so big, but then I did...
#
unique(flights$origin)
# ...and saw that there were only 3 airports and they were all NY airports!
# Tricky!
#
# ...but there were 19 flights with a destination as one of the NYC airports.
# That means that, of the big total, there were
# 19 flights from one NYC airport to another.
#
#
### This is a surprisingly weird question to unpack though...
# Further, suppose we are looking at chartered flightpaths (i.e. flight numbers)
unique(flights[flights$dest %in% NYC, ]["flight"])
# That produces 3,844 unique flightpaths or "flights" originating in NYC.
# There are a lot of different interpretations for this, depending
# on how we define flights. i.e. including particular planes on unique routs, etc.
#
#
#
#
#
#
# b. How many flights were there from NYC airports to Seattle (SEA) in 2013?
# pseudocode for this: select origin=NYC dest=SEA
#
#
flights[flights$dest=="SEA", ]
# There were 3,923 flights originating in NYC airports with SEA as the destination.
#
#
#
#
#
# c. How many airlines fly from NYC to Seattle? Hint: look at the function unique()
# pseudocode for this unique(select carrier WHERE origin=NYC dest=SEA)
#
unique(flights[flights$origin %in% NYC & flights$dest=="SEA", ]["carrier"])
#
# There are 5 unique airlines: AS, DL, UA, B6, and AA.
# I think these are Alaska, Delta, United, British, and American,
# but I didn't look for a key anywhere.
#
#
#
# d. What is the average arrival delay for flights from NYC to Seattle?
# pseudocode for this: avg(select arr_delay WHERE origin=NYC dest=SEA )
#
flightsSEA <- flights[flights$dest=="SEA", ]
mean(flightsSEA$arr_delay, na.rm=T)
# On average, it appears that flights from NYC to SEA are about a minute and 6 seconds early (-1.099min).
# Good work, NY!
#
#
# 2 - Flights 
#
#
#
# a. What is the mean arrival delay time? What is the median arrival delay time?
mean(flights$arr_delay, na.rm=T)
# The mean is 6.89, which means on average planes arrive about 6min53sec late. 
median(flights$arr_delay, na.rm=T)
# The median is -5, which means at the center of the distribution, planes arrive about 5min early.
#
# b. What does a negative arrival delay mean?
# A negative arrival delay might mean a flight showed up ahead of schedule.
# 
# c. Plot a histogram of arrival delay times. Are the answers you obtained in (a) 
# consistent with the shape of the delay time distribution?
hist(flights$arr_delay,
     main="Arrival Delay Times",
     xlab="Delay",
     ylab="Flight Count")
#
# It seems to be pretty consistent, yes. Though the mean is positive overall, it is probably brought
# down by the large number of negative arrival times (a good thing!!).
# Further, the distribution is centered at -5.
# Based on that, it looks like maybe there's a Poisson distribution going on, with most of the
# flights actually arriving early and fewer and fewer arriving with some delay.
#
#
# d. Is there seasonality in departure delays? Describe the patterns. 
plot(by(flights$dep_delay, flights$month, function(x) mean(x, na.rm=T)),
     main="Seasonality of Departure Delays",
     xlab="Calendar Month",
     ylab="Average Delay",
     type="b")
#
#
# There is definitely seasonality for departure delays.
# Peak delays occur during June, July, and jump up in December as well,
# and delays drop down in the autumn. Specifically, summer
# appears to be a bad season for flying, as does Christmas.
# Based on inference from this graph, the best time to leave NY would definitely
# be November and the worst time
# would be July. Inferring based on this graph leads me to believe that a lot
# of people are going through the big NY hubs during holidays (i.e. summer and
# winter time off) and that leads to a lot more congestion/delays.
#
#
# 3 - EDA
#
# a. Plot a histogram for flight times with 100 bins.
# How many peaks do you see in this distribution? What is an explanation for this?
#
hist(flights$air_time,
     breaks=100,
     main="Total Air Time",
     xlab="Flight Time (Minutes?)",
     ylab="Flight Count")
#
# I see five or six different peaks in this distribution. There's some on the low side
# of the distribution
# (probably around an hour of flight time), more clustered around 2-3h of flight time,
# yet another around 5h of flight time, and one far to the right of the distribution.
#
# Quite the long tail, huh?
#
# I would account for these peaks in the following way: the NY airports are major
# hubs, potentially serving an international clientele or domestic audience.
# Most of the flights are about an hour or two.
# A person flying can either terminate or connect to a different
# airport to move onto their destination, so that accounts for
# the sub-2hr flights as ones within the continental US.
# Some of the higher flight-time flights. This data may not account for time zones though?
#
#
#
# b. What time of day do flights most commonly depart? Why might there be two most
# popular times of day to depart?
#
hist(flights$dep_time,
     breaks=24,
     main="Flight Departure Times",
     xlab="Departure Time (hourly, 0hr-23:59hr)",
     ylab="Count")
#
# I used a histogram with 24 breaks to determine that most flights leave in the
# morning and in the afternoon, with a focused peak in the morning and a smooth
# peak in the afternoon/evening. This looks fairly bimodal.
#
# If we're going to generalize further, it seems that most of the flights leave
# either before work (7-10am) or around the end of work (4pm-7pm). This is
# probably because most flights serve business customers primarily and have
# departure times geared around the standard office workday. People probably
# have to fly across country to meet clients and such, especially in a hub like NY.
#
#
# c. Plot a box plot of departure delays and hour of departure.
# What pattern do you see? What is an explanation for this?
#
boxplot(flights$dep_delay~flights$hour,
       range=1.5,
       outline=FALSE,
       main="Flight Time vs. Delay",
       xlab = "Hour of Departure",
       ylab = "Departure Delay [min]")
#
# It seems like, though most flights leave on time and there are some crazy outliers,
# the tendency for a flight to leave not on schedule increases throughout the day. In the morning, it looks like
# a flight either tends to leave on time (+/- a few minutes), but in the afternoon everything goes crazy and there's
# incredibly wide variance. Again, I think this point is related to the previous point, that
# when more traffic is passing through the airport, it becomes less easy to provide reliable service,
# and that means being more flexible with the schedule. For example, maybe air traffic control has to do more
# creative routing in order to get planes out on time in the evening.
#
#
#
#
# d. Develop one research question you can address using the nycflights2013 dataset.
# Provide two visualizations to support your exploration of this question.
# Discuss what you find.
#
# My research question is how do we effectively plan our travel to minimize delays?
# That is, whether or not one of the airports performs worse in terms of delays.
# If there is a worst performing airport,
# is there a worst time to fly out of that airport, and finally, is there a worst airline to fly on?
#
# We looked at the aggregate departure delays for flights, but I am wondering if--during the peak seasons--
# one airport performs better (or worse) than the others. Let's try the following...

LGA <- flights[flights$origin == 'LGA',]
JFK <- flights[flights$origin == 'JFK',]
EWR <- flights[flights$origin == 'EWR',]

plot(by(JFK$dep_delay, JFK$month, function(x) mean(x, na.rm=T)),
     main="Seasonality of Departure Delays by Airport",
     xlab="Calendar Month",
     ylab="Average Delay (min)",
     col="red",
     type="b")
par(new=TRUE)
plot(by(LGA$dep_delay, LGA$month, function(x) mean(x, na.rm=T)),
     xlab="Calendar Month",
     ylab="Average Delay (min)",
     col="blue",
     type="b")
par(new=TRUE)
plot(by(EWR$dep_delay, EWR$month, function(x) mean(x, na.rm=T)),
     xlab="Calendar Month",
     ylab="Average Delay (min)",
     col="green",
     type="b")
#
# Based on this, it looks like EWR is a pretty terrible airport. On average, it has
# more departure delays than the other two in most months, except a handful of
# noteworthy points (July, where everything is terrible, and September, where it's
# least bad of the three).
#
# This is a sanity check, but maybe there are fewer flights that go through the other
# two airports. More flights==more opportunity for delays.
dim(JFK)[1] < dim(EWR)[1]
dim(LGA)[1] < dim(EWR)[1]
#
# Looks like that's true. EWR is bigger, which means more planes, and more delays.
#
# But let's get more granular and see if a there is a particular time where it is worst to leave
# the worst airport.
plot(by(EWR$dep_delay, EWR$hour, function(x) mean(x, na.rm=T)),
     xlab="Departure Hour",
     ylab="Average Delay (min)",
     col="green",
     type="h")
#
# Based on this, it seems that 7pm (19:00h) is the worst time to leave, followed by
# 8pm (20:00h). Perhaps this is due to something we saw in the previous questions, that more
# air traffic congestion occurs right at the end of the standard office workday, when
# people are taking commuter flights home.
#
# Finally, let's see if one particular airline is responsible for delays.
barplot(by(EWR$dep_delay, EWR$carrier, function(x) mean(x, na.rm=T)))
#
# It seems like the carriers EV and OO have the longest delays, while US and AS have the shortest.
# But that might be disingenuous. Let's do a sanity check to see how many flights there are for
# each airline. Let's figure out the percentage.
flightcount <- as.data.frame(table(EWR$carrier))
flightcount$pct <- NA
flightcount$pct <- flightcount$freq/sum(flightcount$Freq)
flightcount$pct <- flightcount$pct *100
flightcount
# And this is more interesting... the carrier EV has a huge amount of delays
# but they are also serving 36% of the flights out of the airport!
# The other three carriers don't even make up 5% of the outgoing flights.
#
# More flights means more potential for delays, but it sucks pretty bad if you
# are a small carrier and also have high delays (like OO, which I am assuming is
# chartered flights or something to that effect).
#
# Basically, what we've learned so far is:
#
# if you fly out of EWR in the evening after work in the summer
# on EV (or on a bigger airline like MQ), you're probably
# going to leave about 20min late on average and you're probably going to have a bad time.
#
# If you want to get to your destination on time, leave really early in the morning in November,
# fly out of LGA (ideally before 9am, to be reasonable),
# and take a small airline. Then you are minimizing opportunities for delays.
#
#