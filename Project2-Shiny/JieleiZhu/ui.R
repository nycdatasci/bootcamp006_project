library(shiny)

shinyUI(
  navbarPage (
    title = 'Test your intuitions on personal income',
    id = 'nav',
    theme = shinytheme('united'),
    
    tabPanel('Motivation',
             fluidRow(
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               h3('How much do you know about personal income in the U.S.?',
                  align = 'center'),
               
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               h3('Not sure?', align = 'center'),
               h3("Well, let's find out!", align = 'center'),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               column(9,
                      h3(' ')
               ),
               column(12,
                      h3('Please answer the following questions about Personal Income in the U.S., and indicate how much money would you bet on each of the answers.'),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      h4('Income vs. Marital Status'),
                      radioButtons("answer1", 
                                   label = h3("Who has the highest income?"),
                                   choices = list("Never married" = 1, "Married" = 2, "Divorced" = 3)
                      ),
                      
                      sliderInput("bet1", 
                                  label = h3("How much would you bet on it?"), 
                                  min = 0, 
                                  max = 100, 
                                  value = 0
                      ),
                      br(),
                      br(),
                      radioButtons("answer2", 
                                   label = h3("Who has the highest income?"),
                                   choices = list("Married, Spouse Absent" = 1, "Divorced" = 2)
                      ),
                      
                      sliderInput("bet2", 
                                  label = h3("How much would you bet on it?"), 
                                  min = 0, 
                                  max = 100, 
                                  value = 0
                      ),
                      br(),
                      br(),
                      h4('Income vs. Gender'),
                      radioButtons("answer3", 
                                   label = h3("Who has the higher income?"),
                                   choices = list("Male" = 1, "Female" = 2)
                      ),
                      
                      sliderInput("bet3", 
                                  label = h3("How much would you bet on it?"), 
                                  min = 0, 
                                  max = 100, 
                                  value = 0
                      ),
                      br(),
                      br(),
                      h4('Income vs. Race'),
                      radioButtons("answer4", 
                                   label = h3("Who has the highest income?"),
                                   choices = list("White" = 1, "Black" = 2, "Asian" = 3, "Hispanic" = 4)
                      ),
                      
                      sliderInput("bet4", 
                                  label = h3("How much would you bet on it?"), 
                                  min = 0, 
                                  max = 100, 
                                  value = 0
                      ),
                      br(),
                      br(),
                      h4('Income vs. Gender & Race'),
                      radioButtons("answer5", 
                                   label = h3("Who has the higher income?"),
                                   choices = list("White female" = 1, "Asian male" = 2)
                      ),
                      
                      sliderInput("bet5", 
                                  label = h3("How much would you bet on it?"), 
                                  min = 0, 
                                  max = 100, 
                                  value = 0
                      )
               )
             )
             
    ),
    tabPanel('Understand the data',
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
    navbarMenu('Explore the data',
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
    ),
    tabPanel('Summary',
             fluidRow(
               column(3, img(src='wedding_money.jpg', height=400, width=300)),
               column(6,
                      h3('1. According to income data from 2005 to 2014, married couples reported the highest income on average, followed by people who are divorced, and people who stayed single.'),
                      br(),
                      br(),
                      h4('2. When breaking down married people into "spouse present" and "spouse absent" subgroups, people who are married but does not live with their spouse reported lower income than people who are divorced.'),
                      br(),
                      br(),
                      h4('3. Men on average have higher income than women.'),
                      br(),
                      br(),
                      h4('4. Among all races, Caucasians and Asians tend to have the highest income, followed by African Americans and Hispanics.'),
                      br(),
                      br(),
                      h4("5. When comparing income level between Asian men and Caucasian women, Asian men earn much more than Caucasian women.")
               ),
               br(),
               br(),
               br(),
               h2("You earned $", textOutput('points', inline = T))
               
             )
    )
  )
)