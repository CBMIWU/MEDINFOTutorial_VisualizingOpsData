library(shiny)
library(shinydashboard)

library(rCharts)

dashboardPage(
  dashboardHeader(
    title = "Demo Dashboard"
  ),
  dashboardSidebar(
    dateRangeInput(inputId = "dateRange", label = "Date Range",
                   start = "2012-01-01", end = Sys.Date(),
                   format = "mm/dd/yyyy")
  ),
  dashboardBody(
    box(
      showOutput("PI_growth_chart", "highcharts")
    ),
    box(
      selectInput(inputId = "group", label = "Group by: ",
                  choices = c("Application", "FundingType"),
                  selected = "Application"),
      plotOutput("FrequencyChart", height = 300)
    ),
    box(
      selectInput(inputId = "distVar", label = "Distribution of: ",
                  choices = c("HoursLogged","DaysOpen"),
                  selected = "HoursLogged"),
      plotOutput("DistributionPlot", height=300)
    )
  )
)
