##################################################
# Visualizing Operational Informatics Data Using R
# MEDFINFO 2015
# by Leslie McIntosh & Connie Zabarouskaya
# Hands-on for rCharts
##################################################

# Source Code (step 0)
library(rCharts)

# read in the data
opsdata <- read.csv("data/opsdata.csv")

# function to convert the date format into suitable for rCharts
to_jsdate2 <- function(x){
  as.numeric(as.POSIXct(as.Date(x), origin="1970-01-01")) * 1000
}

# change the date columns format to Date
opsdata$created <- as.Date(opsdata$created, format = "%m/%d/%Y %H:%M")

# sort the data by date
opsdata <- opsdata[order(opsdata$created),]

# create a month variable for aggregation
opsdata$created_month <- as.Date(cut(opsdata$created, "month"))

# create a vector with cumulative sum of unique PIs
unique_PIs <- cummax(as.numeric(factor(opsdata$PI_name, levels = unique(opsdata$PI_name))))

# matching cumulative sum of unique PIs to unique months
PI_cumul_growth <- aggregate(unique_PIs, list(Month=opsdata$created_month), max)

# change the date format to suit rCharts
PI_cumul_growth$date <- to_jsdate2(as.Date(PI_cumul_growth$Month))

# Helpful Links:
# See full API for JavaScript HighCharts here: http://api.highcharts.com/highcharts
# see more date/time options here: http://api.highcharts.com/highcharts#tooltip.date

# Follow the steps of the hands-on exercise

# After you complete the training steps, move on to your task.

#################################
# Your task
#################################

# You are given 5 min to try and build this chart on your own.
# The dataset is prepared below (Step 0)
valid_app_tickets <- subset(opsdata, Application != "")
Projects_by_product <- aggregate(valid_app_tickets$title, 
                                 by = list(Application = valid_app_tickets$Application),
                                 length)
names(Projects_by_product) <- c("Application", "Projects")
Projects_by_product <- Projects_by_product[order(-Projects_by_product$Projects),]
topProducts <- head(Projects_by_product, (which(Projects_by_product$Application=="Other")-1))
topProducts <- rbind(topProducts, c("Other", sum(tail(Projects_by_product$Projects,-(which(Projects_by_product$Application=="Other")-1))))) 
topProducts$Projects <- as.numeric(topProducts$Projects)

# Step 1
# Task: build a column chart with the first 7 rows of data in topProducts dataset
# Add a clear title to the chart and its axes
# Use type of "category" for x-axis, a make gridLines white
# Change the height to be 300 pixels
# Feel free to add more features from the previous example, like zooming for example

