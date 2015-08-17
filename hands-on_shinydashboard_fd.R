##################################################
# Visualizing Operational Informatics Data Using R
# MEDFINFO 2015
# by Leslie McIntosh & Connie Zabarouskaya
# Hands-on for shinydashboard
##################################################

# # Source Code (step 0)
## app.R ##

library(shiny)
library(shinydashboard)

library(rCharts)

# Data preparation code
# function to convert to POSIXct date format, specifically for line chart
to_jsdate2 <- function(x){
  as.numeric(as.POSIXct(as.Date(x), origin="1970-01-01")) * 1000
}

# read in an altered sample of JIRA data 
opsdata <- read.csv("data/opsdata.csv")

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

# add variable with only new PIs added in each month
PI_cumul_growth$new_pi_num <- c(PI_cumul_growth$x[1], (tail(PI_cumul_growth$x, -1) - head(PI_cumul_growth$x, -1)))

#change the date format to suit rCharts
PI_cumul_growth$date <- to_jsdate2(as.Date(PI_cumul_growth$Month))

# the function handling UI
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

# in a ui.R file it will look like this:
# dashboardPage(
#   dashboardHeader(),
#   dashboardSidebar(),
#   dashboardBody()
# )

# the function handling server side
server <- function(input, output) { }

# in a server.R file it will look like this:
# shinyServer(
#   function(input, output) {}
# )

shinyApp(ui, server)

# This is the canvas you'll use to fill with charts and widgets
# NOTE: this layout is unique to shinydashboard package, however, once you try it
# you are unlikely to go back to the simple layouts of Shiny package

# Follow the steps of the hands-on exercise

# After you complete the training steps, move on to your task.

#################################
# Your task
#################################

# You are given 5 min to try and add a chart and widget on your own.
# Add a histogram chart to this dashboard, in its own box()
# The histogram should visualize distribution of a variable in the opsdata 
# Dataset (you can use HoursLogged column to begin with). Use the FrequencyPlot
# As a starter. Next add a selectInput() widget as we have with "Number of Projects"
# With two options: HoursLogged and DaysOpen. Connect the input and the histogram.


