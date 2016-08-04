library(shiny)
library(threejs)
library(data.table)
library(dplyr)
library(ggplot2)


investment <- fread('investments.csv', na.strings = c('','-'))
investment <- as.data.frame(investment)
investment <- subset(investment,investment$raised_amount_usd > 0)
company_names <- unique(investment$company_name)


shinyUI(navbarPage(theme = 'bootstrap.css', title = "Capital is King",
                   
                   header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  tabPanel("Global Investment",     
    sidebarLayout(
      globeOutput("invest_global_map", width = "100%", height = "600px"),
      absolutePanel(top = 100, left = 50, class = "panel panel-default small",
                    h4("Investments Between 2 contries"),
        # helpText(""),
        sliderInput("N",  
                    "Top relationship by Investment Amount:",
                    min = 1, 
                    max = 1000, 
                    value = c(1,150),
                    step = 50)
        )

  )),
  navbarMenu("Capital Flow",
             tabPanel("Flow In",
                      sidebarLayout(
                        plotOutput('plot_in',width = "100%", height = "600px"),
                        absolutePanel(top = 500, left = 50, class = "panel panel-default small", draggable = T,
                                      radioButtons('in_by','Capital Flow-in',
                                                 list('Total','Average')), actionButton("go", "Download"))
                          )
                      ),
             
             
             
             
             tabPanel("Flow Out",
                      sidebarLayout(
                        plotOutput('plot_out',width = "100%", height = "600px"),
                        absolutePanel(top = 500, left = 50, class = "panel panel-default small", draggable = T,
                                      radioButtons('out_by','Capital Flow-out',
                                                   list('Total','Average')))
                      )
             ),
             
             tabPanel("Domestic Investment",
                      sidebarLayout(
                        plotOutput('plot_do',width = "100%", height = "600px"),
                        absolutePanel(top = 500, left = 50, class = "panel panel-default small", draggable = T,
                                      radioButtons('do_by','Domestic Investment',
                                                   list('Total','Average')))
                      )
             )
             
),

  tabPanel("Company Fundings",     
           sidebarLayout(
             selectInput("company",  # choose the residents
                         label = "Choose A Company",
                         choices = company_names),
             mainPanel(
               dataTableOutput('breakdown'),
               hr()
                       
           )
  )),
  
  tabPanel("Dataset",     
             sidebarLayout(
               h3('Original Dataset'),
               mainPanel(
                 dataTableOutput('table')
               )
             ))

))
?hr()
