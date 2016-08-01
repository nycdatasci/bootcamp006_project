
shinyUI(
  navbarPage (
    title = 'Relationship Between Income and Marital Status',
    id = 'nav',
    theme = shinytheme('United'),
    
    tabPanel('Visualization',
             fluidRow(
               column(3,
                      h4('')
               ),
               column(6,
                      h3('Income vs. Marital status'),
                      box(htmlOutput('marital_motion'), height = 500)
               )
             )
    ),
    
    tabPanel('Raw',
             fluidRow(
               column(
                 width = 2,
                 style = "background-color:#F8F8F8",
                 h4('Explore raw data'),
                 br(),
                 selectInput('marital_status',
                             'Select MARITAL STATUS',
                             choices = list('NeverMarried' = 1,
                                            'Married(Total)' = 2,
                                            'SpousePresent' = 3,
                                            'SpouseAway' = 4,
                                            'Divorced' = 5),
                             selected = 1),
                 br(),
                 selectInput('year',
                             'Select YEAR',
                             choices = list('2005' = 1,
                                            '2006' = 2,
                                            '2007' = 3,
                                            '2008' = 4,
                                            '2009' = 5,
                                            '2010' = 6,
                                            '2011' = 7,
                                            '2012' = 8,
                                            '2013' = 9,
                                            '2014' = 10),
                             selected = 10),
                 br(),
                 selectInput('race',
                             'Select RACE',
                             choices = list('White' = 1,
                                            'Black' = 2,
                                            'Asian' = 3,
                                            'Hispanic' = 4),
                             selected = 1),
                 br(),
                 selectInput('gender',
                             'Select GENDER',
                             choices = list('Male' = 1,
                                            'Female' = 2),
                             selected = 1)
                 
               ),
               column(
                 width = 9,
                 box(htmlOutput('hist'), height = 300)
               )
             )
    ),
    tabPanel('More motion plots...',
             fluidRow(
               column(6, 
                      h3('Income vs. Race'),
                      box(htmlOutput('race_motion'), height = 500)
               ),
               column(6, 
                      h3('Income vs. Gender'),
                      box(htmlOutput('gender_motion'), height = 500)
               ),
               column(12,
                      h3('Income vs Race+Gender'),
                      box(htmlOutput('race_gender_motion'), height = 500)
               )
             )
    ),
    tabPanel('Summary',
             fluidPage(
               column(3, img(src='wedding_money.jpg', height=400, width=300)),
               column(6,
                      h3('1. According to income data from 2005 to 2014, married couples reported the highest income on average, followed by people who are divorced, and people who stayed single.'),
                      br(),
                      br(),
                      h3('2. When breaking down married people into "spouse present" and "spouse absent" subgroups, people who are married but does not live with their spouse reported lower income than people are divorced.')
               )
             )
    )
  )
)
