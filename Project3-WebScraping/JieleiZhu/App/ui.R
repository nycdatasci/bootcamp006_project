library(shiny)

shinyUI(
  navbarPage (
    title = 'Finding deals (Created by Emma(Jielei) Zhu)',
    id = 'nav',
    theme = shinytheme('Readable'),
    
    tabPanel('About',
             fluidRow(
               column(width = 2
                      
               )
             )
    ),
    navbarMenu('Past Data',
               tabPanel('Clothing, Jewelry & Bags',
                        fluidRow(
                          column(width = 6
                          )
                        )
               ),
               tabPanel('Beauty',
                        fluidRow(
                          column(width = 6
                          )
                        )
               ),
               tabPanel('Nutrition',
                        fluidRow(
                          column(width = 6
                          )
                        )
               ),
               tabPanel('Baby',
                        fluidRow(
                          column(width = 6
                          )
                        )
               ),
               tabPanel('At Home',
                        fluidRow(
                          column(width = 6
                          )
                        )
               ),
               tabPanel('Electronics',
                        fluidRow(
                          column(width = 6
                          )
                        )
               ),
               tabPanel('Travel',
                        fluidRow(
                          column(width = 6
                          )
                        )
               ),
               tabPanel('Finance',
                        fluidRow(
                          column(width = 6
                          )
                        )
               )
    ),
    tabPanel('Prediction',
             fluidRow(
               column(width = 2
                      
               )
             )
    )
  )
)