library(shiny)

shinyUI(
  navbarPage (
    title = 'Explore your confidence using U.S. Income dataset',
    id = 'nav',
    theme = shinytheme('United'),

    tabPanel('Understand data',
             fluidRow(
               column(
                 width = 2,
                 style = "background-color:#F8F8F8",
                 h4('Explore raw data'),
                 br(),
                 selectInput('marital_status',
                             'Select MARITAL STATUS',
                             choices = marital_choice,
                             selected = 'NeverMarried'),
                 br(),
                 selectInput('year',
                             'Select YEAR',
                             choices = year_choice,
                             selected = '2014'),
                 br(),
                 selectInput('race',
                             'Select RACE',
                             choices = race_choice,
                             selected = 'White'),
                 br(),
                 selectInput('gender',
                             'Select GENDER',
                             choices = gender_choice,
                             selected = 'Male')
               ),
               column(
                 width = 9,
                 htmlOutput('hist')
               )
             )
    ),
    navbarMenu('Explore data',
               tabPanel('Income vs. Marital Status',
                        fluidRow(
                          column(3,
                                 h3(' ')
                          ),
                          column(6,
                                 h3('Income vs. Marital Status'),
                                 htmlOutput('marital_total_motion')
                          )
                        )
               ),
               tabPanel('Income vs. Married(break down)',
                        fluidRow(
                          column(3,
                                 h3(' ')
                          ),
                          column(6,
                                 h3('Income vs. Marital Status'),
                                 htmlOutput('marital_motion')
                          )
                        )
               ),
               tabPanel('Income vs. Gender',
                        fluidRow(
                          column(3,
                                 h3(' ')
                          ),
                          column(6,
                                 h3('Income vs. Gender'),
                                 htmlOutput('gender_motion')
                          )
                        )
               ),
               tabPanel('Income vs. Race',
                        fluidRow(
                          column(3,
                                 h3(' ')
                          ),
                          column(6,
                                 h3('Income vs. Race'),
                                 htmlOutput('race_motion')
                          )
                        )
               ),
               tabPanel('Income vs. Gender & Race',
                        fluidRow(
                          column(3,
                                 h3(' ')
                          ),
                          column(6,
                                 h3('Income vs. Gender & Race'),
                                 htmlOutput('race_gender_motion')
                          )
                        )
               )
    )
  )
)

# tabPanel('Summary',
#          fluidPage(
#            column(3, img(src='wedding_money.jpg', height=400, width=300)),
#            column(6,
#                   h3('1. According to income data from 2005 to 2014, married couples reported the highest income on average, followed by people who are divorced, and people who stayed single.'),
#                   br(),
#                   br(),
#                   h3('2. When breaking down married people into "spouse present" and "spouse absent" subgroups, people who are married but does not live with their spouse reported lower income than people are divorced.')
#            )
#          )
# )
# )
# )

