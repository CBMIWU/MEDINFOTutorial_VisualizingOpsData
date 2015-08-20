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

# Step 1
# Let's plot a time series with a single line of code, using HighCharts library
# See full API for JavaScript HighCharts here: http://api.highcharts.com/highcharts
# This is the simplest format that the hPlot chart function needs
PI_growth_plot <- hPlot(x ~ date, type = "line", data = PI_cumul_growth)
PI_growth_plot
# This doesn't look very attractive and look at those weird numbers on x-axis...

# Step 2
# Fix the values in x-axis - make them date/time format
# Also add proper label to the x-axis
# Notice the dollar-sign notation (as the object of plot is essentially a list)
PI_growth_plot <- hPlot(x ~ date, type = "line", data = PI_cumul_growth)
PI_growth_plot$xAxis(type='datetime', title = list(text = "Time"))
PI_growth_plot
# You may notice that the chart y-axis starts below zero, which is non proper
# Also the label for y-axis needs clarification
# And let's say you don't like the grid lines and want to add the chart a clean look

# Step 3
# To resolve this let's add an attribute specification for y-axis, similar to x-axis
# specify the label for the axis, the minimum value to start the chart from and the
# color for gridlines to be white (there's no other way to disable them)
PI_growth_plot <- hPlot(x ~ date, type = "line", data = PI_cumul_growth)
PI_growth_plot$xAxis(type='datetime', title = list(text = "Time"))
PI_growth_plot$yAxis(title = list(text = "PIs"),
                     min = 0, gridLineColor = "#ffffff")
PI_growth_plot
# It looks much better but what if you don't like the dots on the line and we don't have a title

# Step 4
# To get rid of the dots on the line, need to specify plotOptions
# In the plotOptions section you can also change the look of the line, like its color
# You can add title in two ways, one way is to include it in the hPlot function
PI_growth_plot <- hPlot(x ~ date, type = "line", data = PI_cumul_growth, title = "Number of PIs")
PI_growth_plot$xAxis(type='datetime', title = list(text = "Time"))
PI_growth_plot$yAxis(title = list(text = "PIs"),
                     min = 0, gridLineColor = "#ffffff")
PI_growth_plot$plotOptions(line = list(color = "#5C7A00", marker = list(enabled = F)))
PI_growth_plot

# this looks much better. Now let's imagine your time series chart was more precise 
# you want it to show the exact day, not just the month in the tooltip

# Step 5
# Add tooltip attributed with a specific date/time format
# see more date/time options here: http://api.highcharts.com/highcharts#tooltip.date
PI_growth_plot <- hPlot(x ~ date, type = "line", data = PI_cumul_growth, title = "Number of PIs")
PI_growth_plot$xAxis(type='datetime', title = list(text = "Time"))
PI_growth_plot$yAxis(title = list(text = "PIs"),
                     min = 0, gridLineColor = "#ffffff")
PI_growth_plot$plotOptions(line = list(color = "#5C7A00", marker = list(enabled = F)))
PI_growth_plot$tooltip(dateTimeLabelFormats = list(month = "%A, %b %e, %Y"))
PI_growth_plot

# Step 6
# Lastly let's add a zooming feature to this chart and change it's height
# For that, use chart attribute
PI_growth_plot <- hPlot(x ~ date, type = "line", data = PI_cumul_growth, title = "Number of PIs")
PI_growth_plot$xAxis(type='datetime', title = list(text = "Time"))
PI_growth_plot$yAxis(title = list(text = "PIs"),
                     min = 0, gridLineColor = "#ffffff")
PI_growth_plot$plotOptions(line = list(color = "#5C7A00", marker = list(enabled = F)))
PI_growth_plot$tooltip(dateTimeLabelFormats = list(month = "%A, %b %e, %Y"))
PI_growth_plot$chart(zoomType="x", height = 300)
PI_growth_plot

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



# Solution
mostPopularProduct <- hPlot(Projects ~ Application, type = 'column', data = topProducts, 
                            title = "Most Popular Product")
mostPopularProduct$xAxis(type = "category", title = list(text = "Application"))
mostPopularProduct$yAxis(title = list(text = "Number of Projects"), gridLineColor = "#ffffff")
mostPopularProduct$chart(height = 300)
mostPopularProduct

# Advanced
# Step 2
# Enhance the chart by adding a clear tooltip
mostPopularProduct <- hPlot(Projects ~ Application, type = 'column', data = topProducts, 
                            title = "Most Popular Product")
mostPopularProduct$xAxis(type = "category", title = list(text = "Application"))
mostPopularProduct$yAxis(title = list(text = "Number of Projects"), gridLineColor = "#ffffff")
mostPopularProduct$tooltip(formatter = "#! function() { return this.point.y + ' Projects'; } !#")
mostPopularProduct$chart(height = 300)
mostPopularProduct
