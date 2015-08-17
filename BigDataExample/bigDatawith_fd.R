#################################################################
# install.packages('ff')
library(ff)

# install.packages('ffbase')
library(ffbase)

#################################################################
# stats on flights from http://stat-computing.org/dataexpo/2009/the-data.html
# reading in the dataset in increments
# using ffbase function
flights2008 <- read.table.ffdf(file="2008.csv.bz2", FUN = "read.csv", na.strings = NA,
                           VERBOSE=TRUE, first.rows=100000, next.rows=50000)


#################################################################

class(flights2008)
# [1] "ffdf"
dim(flights2008)
# [1] 7009728      29
names(flights2008)
str(flights2008[1:29,])
# do not just call str(flights2008), unless you want weird output
# dplyr functions do not seem to work on ffdf format
# but aggregate does
meanDelays <- aggregate(DepDelay ~ UniqueCarrier, data=flights2008, mean)
head(meanDelays[order(meanDelays$DepDelay,decreasing=TRUE),])

#################################################################
# what airline had the most flights that year?
carriers <- as.data.frame(table(flights2008$UniqueCarrier))

carriers <- carriers[order(carriers$Freq, decreasing=TRUE),]
carriers[1,]
# WN - Southwest Airlines

# install.packages("googleVis")
suppressPackageStartupMessages(library(googleVis))
carrierChart <- gvisColumnChart(carriers, xvar = "Var1", yvar = "Freq")
plot(carrierChart)

#################################################################


# Let's see if the departure times are uniformly distributed
# it's best to use a static plotting mechanism (ggplot2 or base)
# in base
hist(flights2008$DepDelay[!is.na(flights2008$DepDelay)], col = "steelblue", main = "Departure Delay's histogram", xlab = "DepDelay")

# in ggplot2
library(ggplot2)
# due to ggplot2 API, we have to get creative
delays <- as.data.frame(flights2008[,"DepDelay"])
names(delays) <- "DepDelay"
delayHist <- ggplot(delays, aes(DepDelay)) + geom_histogram(binwidth=5)
delayHist

# because googleVis and rCharts are JavaScript based, plotting over a mln points will most likely fail
# delayHist <- gvisHistogram(as.data.frame(flights2008[,"DepDelay"]), option=list(title="Departure Delay's histogram",
#                                         legend="{ position: 'none' }",
#                                         colors="['green']"))


########################################################
# Some tips and tricks
## how to use %in% operator, and create a new variable
flights2008$DesALB_ABQ <- flights2008$Dest %in% ff(factor(c("ALB","ABQ")))
head(flights2008$DesALB_ABQ)


########################################################
# convert ffdf into a regular dataframe
# install.packages("nycflights13")
# install.packages("dplyr")
library(nycflights13)
library(dplyr)
copy <- as.data.frame(flights2008)
carriers2 <- copy %>% group_by(UniqueCarrier) %>% summarise(Freq = length(UniqueCarrier))
memory.size()
# [1] 978.4

# merging with another data.frame
merged <- left_join(copy, airlines, by = c("UniqueCarrier" = "carrier"))

carriers3 <- merged %>% group_by(name) %>% summarise(Freq = length(name))
carriers3 <- carriers3[order(carriers3$Freq, decreasing=TRUE),]
head(carriers3)
########################################################

# using bigglm function from biglm package for logistic regression
# install.packages("biglm")
library(biglm)
predictingCancellation <- bigglm.ffdf(Cancelled ~ DepDelay, data = flights2008, 
                                       family = binomial(), chunksize = 500000, maxit = 25)


summary(predictingCancellation)
########################################################

