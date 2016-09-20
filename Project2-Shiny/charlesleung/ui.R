#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(DT)

cols = c('PUMA', 'Property Value', 'Household Income', 'Household Type (Married)', 'Sex (Male)', 'Race (White)', 'Education (Higher)')

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    titlePanel("New York Demographics"),
    h5('Can we understand community development by looking at demographic data? The data provided is from the American Community Survey (ACS) for the years 2005 - 2014.'),

  # Create a new row for the table.
#  sidebarLayout(
    # sidebarPanel(
    #   selectInput(
    #     'var',
    #     'Select variable from dataset:',
    #     choices = cols,
    #     br()
    #   )
    # ),
    # position = 'right',
#  )
  mainPanel(
    tabsetPanel(
      type = 'tab',
      tabPanel('Dictionary', h3('Public Use Microdata Areas'), 
               h5('PUMAs are 5-digit codes, used to describe population zones of around 100,000 people.
                  It was first developed for use in the Census and the American Community Survey (ACS).
                  There is a cooperative program between the Census Bureau and the states
                  that allows local input to suggest boundaries for them.'), br(),
               DT::dataTableOutput("table")),
      tabPanel('Comparison', h3('Trending Demographics'),
               h5('For the 2005-2014 period, you can select variables to track amongst the New York Population'), br(),
               #plotOutput('mylines')
               lineChartOutput("mychart"),
               fluidRow(
                 column(width=6,
                        column(4,
                               selectInput("first",
                                           "PUMA 1:",
                                           c(Incomes$PUMA))
                        )
                 ),
                 column(width=6,
                        column(4,
                               selectInput("second",
                                           "PUMA 2:",
                                           c(Incomes$PUMA))
                        )
                 ),
                 column(width=6,
                        column(4,
                               selectInput("third",
                                           "PUMA 3:",
                                           c(Incomes$PUMA))
                        )
                 ),
                 column(width=6,
                        column(4,
                               selectInput("fourth",
                                           "PUMA 4:",
                                           c(Incomes$PUMA))
                        )
                 )
               )
      ),
      tabPanel('Variable Map', plotOutput("heat"),
               selectInput('mapvar', choices = colnames(ACS_P), label = 'Variable', selected = 'RACE_W'),
               sliderInput('timeframe', min = 2005, max = 2014, value = 2010, label = 'Year')
               )
      )
      )
  # Create a new row for Line chart showing Household income change, property Value, higher education
  
  
  ))
