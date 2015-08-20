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

# Step 1
# Let's add a title and a content box to the dashboard 
# Content boxes control the layout of the UI and can be used 
# for text or to output a plot/table/value from the server side
# We already prepared some charts in rCharts hands-on, let's use 
# one for this box (copy the chart from your exercise or solutions file)

ui <- dashboardPage(
  dashboardHeader(
    title = "Demo dashboard"
  ),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        showOutput("PI_growth_chart", "highcharts")
      )
    )
  )
)


# the function handling server side
server <- function(input, output) {
  
  output$PI_growth_chart <- renderChart({
    PI_growth_plot <- hPlot(x ~ date, type = "line", data = PI_cumul_growth, title = "Number of PIs")
    PI_growth_plot$xAxis(type='datetime', title = list(text = "Time"))
    PI_growth_plot$yAxis(title = list(text = "PIs"),
                         min = 0, gridLineColor = "#ffffff")
    PI_growth_plot$plotOptions(line = list(color = "#5C7A00", marker = list(enabled = F)))
    PI_growth_plot$tooltip(dateTimeLabelFormats = list(month = "%A, %b %e, %Y"))
    PI_growth_plot$chart(zoomType="x", height = 300)
    PI_growth_plot$addParams(dom="PI_growth_chart")
    return(PI_growth_plot)
  })
}

shinyApp(ui, server)

# Step 2
# Let's add an input widget. 
# In order to demonstrate a simple dropdown widget, let's add another plot 
# on the server side, which will take input from the user
# The user will select a grouping variable from the dropdown widget, which 
# will be used in grouping the number of projects

ui <- dashboardPage(
  dashboardHeader(
    title = "Demo dashboard"
  ),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        showOutput("PI_growth_chart", "highcharts")
      ),
      box(
        selectInput(inputId = "group", label = "Group by: ",
                    choices = c("Application","FundingType"),
                    selected = "Application"),
        plotOutput("FrequencyPlot", height=300)
      )
    )
  )
)


# the function handling server side
# remember the dataset is prepared at the top of this file and is loaded in memory
server <- function(input, output) {
  output$PI_growth_chart <- renderChart({
    PI_growth_plot <- hPlot(x ~ date, type = "line", data = PI_cumul_growth, title = "Number of PIs")
    PI_growth_plot$xAxis(type='datetime', title = list(text = "Time"))
    PI_growth_plot$yAxis(title = list(text = "PIs"),
                         min = 0, gridLineColor = "#ffffff")
    PI_growth_plot$plotOptions(line = list(color = "#5C7A00", marker = list(enabled = F)))
    PI_growth_plot$tooltip(dateTimeLabelFormats = list(month = "%A, %b %e, %Y"))
    PI_growth_plot$chart(zoomType="x", height = 300)
    PI_growth_plot$addParams(dom="PI_growth_chart")
    return(PI_growth_plot)
  })
  
  output$FrequencyPlot <- renderPlot({
    GROUP <- input$group
    freqPlot <- plot(opsdata[,GROUP], main = "Number of Projects")
    freqPlot
  })
}

shinyApp(ui, server)


# Step 3
# Let's add another widget - a time filter, which will limit the amount of 
# data used in the time series chart. 
# This example will also demonstrate reactive datasets in Shiny, which can 
# be reused in several charts or other graphical objects

ui <- dashboardPage(
  dashboardHeader(
    title = "Demo dashboard"
  ),
  dashboardSidebar(
    dateRangeInput(inputId="DateRange", label="Date Range", start="2012-01-01", 
                   end=Sys.Date(), format="mm/dd/yyyy")
  ),
  dashboardBody(
    fluidRow(
      box(
        showOutput("PI_growth_chart", "highcharts")
      ),
      box(
        selectInput(inputId = "group", label = "Group by: ",
                    choices = c("Application","FundingType"),
                    selected = "Application"),
        plotOutput("FrequencyPlot", height=300)
      )
    )
  )
)


# the function handling server side
# remember the dataset is prepared at the top of this file and is loaded in memory
server <- function(input, output) {
  
  filteredData <- reactive({
    filtered_PI_growth <- subset(PI_cumul_growth, Month >= input$DateRange[1] & 
                                   Month <= input$DateRange[2])
    filtered_PI_growth
  })
  
  output$PI_growth_chart <- renderChart({
    PI_growth_plot <- hPlot(x ~ date, type = "line", data = filteredData(), title = "Number of PIs")
    PI_growth_plot$xAxis(type='datetime', title = list(text = "Time"))
    PI_growth_plot$yAxis(title = list(text = "PIs"),
                         min = 0, gridLineColor = "#ffffff")
    PI_growth_plot$plotOptions(line = list(color = "#5C7A00", marker = list(enabled = F)))
    PI_growth_plot$tooltip(dateTimeLabelFormats = list(month = "%A, %b %e, %Y"))
    PI_growth_plot$chart(zoomType="x", height = 300)
    PI_growth_plot$addParams(dom="PI_growth_chart")
    return(PI_growth_plot)
  })
  
  output$FrequencyPlot <- renderPlot({
    GROUP <- input$group
    freqPlot <- plot(opsdata[,GROUP], main = "Number of Projects")
    freqPlot
  })
}


shinyApp(ui, server)


#################################
# Your task
#################################

# You are given 5 min to try and add a chart and widget on your own.
# Add a histogram chart to this dashboard, in its own box()
# The histogram should visualize distribution of a variable in the opsdata 
# dataset (you can use HoursLogged column to begin with). Use the FrequencyPlot
# as a starter. Next add a selectInput() widget as we have with "Number of Projects"
# with two options: HoursLogged and DaysOpen. Connect the input and the histogram.


# Solution

ui <- dashboardPage(
  dashboardHeader(
    title = "Demo dashboard"
  ),
  dashboardSidebar(
    dateRangeInput(inputId="DateRange", label="Date Range", start="2012-01-01", 
                   end=Sys.Date(), format="mm/dd/yyyy")
  ),
  dashboardBody(
    fluidRow(
      box(
        showOutput("PI_growth_chart", "highcharts")
      ),
      box(
        selectInput(inputId = "group", label = "Group by: ",
                    choices = c("Application","FundingType"),
                    selected = "Application"),
        plotOutput("FrequencyPlot", height=300)
      ),
      box(
        selectInput(inputId = "distVar", label = "Distribution of: ",
                    choices = c("HoursLogged","DaysOpen"),
                    selected = "HoursLogged"),
        plotOutput("DistributionPlot", height=300)
      )
    )
  )
)


# the function handling server side
# remember the dataset is prepared at the top of this file and is loaded in memory
server <- function(input, output) {
  
  filteredData <- reactive({
    filtered_PI_growth <- subset(PI_cumul_growth, Month >= input$DateRange[1] & 
                                   Month <= input$DateRange[2])
    filtered_PI_growth
  })
  
  output$PI_growth_chart <- renderChart({
    PI_growth_plot <- hPlot(x ~ date, type = "line", data = filteredData(), title = "Number of PIs")
    PI_growth_plot$xAxis(type='datetime', title = list(text = "Time"))
    PI_growth_plot$yAxis(title = list(text = "PIs"),
                         min = 0, gridLineColor = "#ffffff")
    PI_growth_plot$plotOptions(line = list(color = "#5C7A00", marker = list(enabled = F)))
    PI_growth_plot$tooltip(dateTimeLabelFormats = list(month = "%A, %b %e, %Y"))
    PI_growth_plot$chart(zoomType="x", height = 300)
    PI_growth_plot$addParams(dom="PI_growth_chart")
    return(PI_growth_plot)
  })
  
  output$FrequencyPlot <- renderPlot({
    GROUP <- input$group
    freqPlot <- plot(opsdata[,GROUP], main = "Number of Projects")
    freqPlot
  })
  
  output$DistributionPlot <- renderPlot({
    VAR <- input$distVar
    histPlot <- hist(opsdata[,VAR], main = paste0("Distribution of ", VAR),
                     xlab = VAR)
    histPlot
  })
}


shinyApp(ui, server)

# Step 6 and Last Step
# Let's separate this code into two files as you would in a Shiny application.
# If you wish to use some elements from the processed data not just in your server file
# but also in your UI file, make sure to put all preliminary data processing into a 
# global.R file and it will be visible to both ui.R and server.R