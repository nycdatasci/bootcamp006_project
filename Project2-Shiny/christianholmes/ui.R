library(shiny)
library(plotly)

shinyUI(fluidPage(
  navbarPage(
    theme = shinytheme('spacelab'),
    title = 'Driver Safety in the US',
    
    tabPanel('Home',
      mainPanel(
        div(style = 'margin-left: 15%;',
            br(),
            br(),
            h2('Welcome!'),
            br(),
            br(),
            h3('About This Project'),
            p('This app allows you to visualize data about driving fatalities from the past 15 years in the US. The data was taken from the Fatality Analysis Report System (FARS) dataset, which is distributed by the National Highway Traffic Safety Administration (NHTSA) every year.'),
            br(),
            br(),
            h3('What Variables Does The Dataset Include?'),
            p("The FARS report contains a wide variety of variables about the mechanics of the crash, the location, and the passengers involved. It also contains data regarding the blood alcohol content of the driver, whether the driver was distracted, and whether either party was speeding. We'll focus on those variables for this app."),
            br(),
            br(),
            h3('Why Is This Important?'),
            p("Driving is the most dangerous thing most of us do daily, yet it is rarely treated as such. Roughly 32,500 people died in car accidents last year, and it was the number one cause of death of death for those aged 15-24. As a point of comparison, roughly 15,000 people died as a result of homicide, yet homicide reduction received far more media and political attention."),
            br(),
            br(),
            h3('What Can You Do With This App?'),
            p("You can investigate year over year trends in driving fatalities, or you can see a map of driving fatality rates by state. If you'd like to see how your state compares to other states, check out the final tab, State Comparisons."),
            br(),
            br(),
            h3('Thanks So Much For Visiting!'),
            p("And remember, please drive safely."
              )
            )
      )
    ), # End of first tab
  tabPanel("Year Over Year",
           sidebarLayout(
             sidebarPanel(h3("Yearly Fatality Rate"), style = "width:300px",
                          helpText("This graph displays the yearly changes in driving fatalities. You can customize this graph by viewing subsets of the total deaths related to drunk driving, distracted driving, or speeding."),
                          br(),
                          radioButtons("lineG", h4("Filters"), choices = 
                                               list("Total" = 'total', "Drunk" = 'drunk', "Distracted" = 'distract', "Speeding" = 'speeding'), selected = "Total"),
                          br(),
                          sliderInput("lineRange", h4("Years of Interest:"), sep = "",
                                      min = 1999, max = 2014, value = c(1999, 2014))
                          ),
             mainPanel(style="position:relative",
                       h3("Driving Fatalities", style="margin:20px; margin-left:200px;  z-index:100; position:absolute"),
                       br(),
                       br(),
                       plotlyOutput("trendPlot"),
                       htmlOutput("plotP", style="margin-left:-100px; margin-top:-80px")
             )
             )
),

tabPanel("Map",
         sidebarLayout(
           sidebarPanel(h3("Map"), style = "width:300px",
                        helpText("This map displays the driving fatality rate by state. It is calculated by diving the total numbers of driving fatalities in that state by the population of the state, and multiplying by 100,000."),
                        br(),
                        radioButtons("map", label = h4("Filters"),
                                     choices = list("Total" = "total", "Drunk" = "drunk",
                                                    "Distracted" = "distract", "Speeding" = "speeding"),selected = 'total'),
                        br(),
                        sliderInput("mapRange", h4("Years of Interest:"), sep = "",
                                    min = 1999, max = 2014, value = c(1999, 2014))
                        ),
           mainPanel(style="position:relative",
                     h3("US Fatality Rate Map (Average)", style="margin:20px; margin-left:200px;
              z-index:100; position:absolute"),
                     br(),
                     br(),
                     plotlyOutput("map1"),
                     htmlOutput("plotS", style="margin:-100px; margin-left:-80px;")
           )
         )
),


tabPanel("State Comparison",
         sidebarLayout(
           sidebarPanel(h3("State Comparison (Average)"), style = "width:300px",
                        helpText("This graph allows you to compare driving fatality rates by state."),
                        br(),
                        fluidRow(
                               selectInput("stateOne", label = h4("State 1"), 
                                           choices = list("Alabama" = 1, "Alaska" = 2,
                                                          "Arizona" = 4, "Arkansas" = 5, "California" = 6, "Colorado" = 8, "Connecticut" = 9, "Delaware" = 10, "Florida" = 12, "Georgia" = 13, "Hawaii" = 15, "Idaho" = 16, "Illinois" = 17, "Indiana" = 18, "Iowa" = 19, "Kansas" = 20, "Kentucky" = 21, "Louisiana" = 22, "Maine" = 23, "Maryland" = 24, "Massachusetts" = 25, "Michigan" = 26, "Minnesota" = 27, "Mississippi" = 28, "Missouri" = 29, "Montana" = 30, "Nebraska" = 31, "Nevada" = 32, "New Hampshire" = 33, "New Jersey" = 34, "New Mexico" = 35, "New York" = 36, "North Carolina" = 37, "North Dakota" = 38, "Ohio" = 39, "Oklahoma" = 40, "Oregon" = 41, "Pennsylvania" = 42, "Rhode Island" = 44, "South Carolina" = 45, "South Dakota" = 46, "Tennessee" = 47, "Texas" = 48, "Utah" = 49, "Vermont" = 50, "Virginia" = 51, "Washington" = 53, "West Virignia" = 54, "Wisconsin" = 55, "Wyoming" = 56), selected = 1)),
                        fluidRow(
                               selectInput("stateTwo", label = h4("State 2"), 
                                           choices = list("Alabama" = 1, "Alaska" = 2,
                                                          "Arizona" = 4, "Arkansas" = 5, "California" = 6, "Colorado" = 8, "Connecticut" = 9, "Delaware" = 10, "Florida" = 12, "Georgia" = 13, "Hawaii" = 15, "Idaho" = 16, "Illinois" = 17, "Indiana" = 18, "Iowa" = 19, "Kansas" = 20, "Kentucky" = 21, "Louisiana" = 22, "Maine" = 23, "Maryland" = 24, "Massachusetts" = 25, "Michigan" = 26, "Minnesota" = 27, "Mississippi" = 28, "Missouri" = 29, "Montana" = 30, "Nebraska" = 31, "Nevada" = 32, "New Hampshire" = 33, "New Jersey" = 34, "New Mexico" = 35, "New York" = 36, "North Carolina" = 37, "North Dakota" = 38, "Ohio" = 39, "Oklahoma" = 40, "Oregon" = 41, "Pennsylvania" = 42, "Rhode Island" = 44, "South Carolina" = 45, "South Dakota" = 46, "Tennessee" = 47, "Texas" = 48, "Utah" = 49, "Vermont" = 50, "Virginia" = 51, "Washington" = 53, "West Virignia" = 54, "Wisconsin" = 55, "Wyoming" = 56), selected = 2)),
                        radioButtons("stateradio", label = h4("Filters"),
                                     choices = list("Total" = 'total_rate', "Drunk" = 'drunk_rate',
                                                    "Distracted" = 'distract_rate', "Speeding" = 'speeding_rate'), selected = 'total_rate'),
                        sliderInput("barRange", h4("Years of Interest:"), sep = "",
                                    min = 1999, max = 2014, value = c(1999, 2014))
           ),
           mainPanel(style="position:relative",
                     h3("State Comparison", style="margin:20px; margin-left:200px;
              z-index:100; position:absolute"),
                     br(),
                     br(),
                     plotlyOutput("bar_chart"),
                     htmlOutput("plotR", style="margin-left:-100px; margin-top:-80px")
           )
         )
)

)))
      