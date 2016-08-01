library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "Terrorism Trends"),
  dashboardSidebar(
    sidebarUserPanel("Author: Sharan Duggal"),
    sidebarMenu(menuItem("About this Project", tabName = "Info", icon = icon("info-circle")),
                menuItem("Relative Impact", tabName = "TreeMap", icon = icon("area-chart")),
                menuItem("Attack Trends", tabName = "TrendChart", icon = icon("line-chart")),
                menuItem("Series Comparison", tabName = "BarChart", icon = icon("bar-chart")),
                menuItem("Country Comparison", tabName = "ComparisonChart", icon = icon("flag-o")),
                menuItem("Map", tabName = "map", icon = icon("map")),
                

    selectizeInput("Year", "Year of Incident", c("All", sort(unique(data$iyear))),
                   selected = NULL),
    
    selectizeInput("Reg", "Region", c("All", sort(unique(data$region_txt)),
                                      selected = NULL)), 
    
    selectizeInput("Ctry", "Country", c("All", sort(unique(data$country_txt)),
                   selected = NULL))

  )),
  
  
  dashboardBody(
         tabItems(
           tabItem(tabName = "Info",
                   column(12,
                          tabsetPanel(
                            tabPanel("Motivations & Objectives",
                                     fluidRow(
                                       column(12,
                                              h3('Objectives'),
                                              p("This Shiny application was developed by Sharan Duggal as a project during his stint at NY Data Science Academy Bootcamp 6."),
                                              p("On March 12th, 1993, I was sitting in class and I hear a loud explosion that rattles the school's windows. No, it's not an ordinary
                                                  fire cracker, but a bomb targeted at the Mumbai stock exchange. As it turns out, it was one of 13 bombs too simultaneously go off
                                                  in the city at the same time. It was the biggest coordinated attack of its kind on Indian soil."),
                                              p("Dealing with terrorist activities has been a regular part of our lives -- definitely more so for some countries than others -- since
                                                probably as far as the history books go, but there definitely seems to be an increase of such activity in the recent past,
                                                and even the nature of such events seem to be constantly changing."),
                                              p("This has inspired a passion project to build an app that would help the reader:"),
                                              tags$ul(
                                                tags$li("Analyze terrorist activities over time and across regions of the world"),
                                                tags$li("Analyze trends in the types of attacks, weapons being used among other information"),
                                                tags$li("Visualize spatial information about the events to attempt to identify higher risk cities, and eventually, potentially higher
                                                        risk targets for better allocation of resources to help prevent such events.")
                                                    )
                                              )
                                              )
                                    ),
                            tabPanel("About the data",
                                     fluidRow(
                                       column(4,
                                              h3("Data Source"),
                                              p("All data has been sourced from the Global Terrorism Database (GTD), which is being managed by START, an organization
                                                headquartered at the University of Maryland. The data set I used for this app is the entire database downloaded as of
                                                July 21st, 2016,  and contains close to 157,000 records."),
                                              h3("Origins of Data Collection"),
                                              p("The original set of incidents that comprise the GTD occurred between 1970 and 1997 and were
                                                collected by the Pinkerton Global Intelligence Service (PGIS)—a private security agency. After
                                                START completed digitizing these handwritten records in 2005, they collaborated with the Center
                                                for Terrorism and Intelligence Studies (CETIS) to continue data collection beyond 1997 and
                                                expand the scope of the information recorded for each attack. CETIS collected GTD data for
                                                terrorist attacks that occurred from January 1998 through March 2008, after which ongoing
                                                data collection transitioned to the Institute for the Study of Violent Groups (ISVG). ISVG
                                                continued as the primary collector of data on attacks that occurred from April 2008 through
                                                October 2011. Beginning with cases that occurred in November 2011, all ongoing GTD data collection is
                                                conducted by START staff at the University of Maryland. GTD staff at START retroactively coded several key variables not
                                                originally available for the PGIS cases, conducted numerous quality control projects, and supplemental data collection efforts."),
                                              p("As information collection changes in terms of ownership, as well as in terms of methodologies used - with improving technologies
                                                and more readily available digitized information - despite anyone's best efforts there is likely to be some bias in the amount of
                                                informaion available. The distribution on the right shows this to potentially be the case."),
                                              p("Recent years have shown strong spikes in the number of events recorded with newer methodologies like machine learning and natural language
                                                processing algorithms being applied on the data. Additionally, in the second chart to the right, three of the years where the responsibility for
                                                information collection changed hands, show either strong spikes or a strong dip in the proportion of results compared to the numbers of the previous
                                                year."),
                                              p(strong( "As such, for this project, rather than focus on raw numbers, I have focused comparing relative levels of metrics by using proportions")),
                                              p("For additional information on the database, key data definitions, methodologies used and othr caveats about the data, please reference:"),
                                              (strong(a("GTD Codebook Download", href='https://www.start.umd.edu/gtd/downloads/Codebook.pdf')))
                                              ),
                                       column(7,
                                              h3("Overall Distribution"),
                                              plotOutput("overall.distribution"),
                                              plotOutput("overall.distribution.change")
                                             )
                                     )
                                      )
                                      )
                          )
                   ),
           tabItem(tabName = "TreeMap",
              fluidRow(
                      htmlOutput("tree", height = 800, wwidth = 1200) #, width = '100%')
                      #,
                # column(3, offset = 1,
                #        htmlOutput("tree.table")
                #        )
              ),
              fluidRow(
                htmlOutput("tree.table")
              )
            ),
            tabItem(tabName = "TrendChart",
                 fluidPage(

                   plotOutput('trend'),

                   hr(),
                   fluidRow(
                     column(3,
                            checkboxGroupInput("INTS", "Internationally Motivated",
                                               International,
                                               selected = c("INT.LOG", "INT.IDEO")),
                            checkboxGroupInput("TargetTypes", "Target", TargetChoices, selected = NULL)
                     ),
                     column(4, offset = 1,
                            checkboxGroupInput("WeaponType", "Type of Weapon Used",
                                  WeaponFilters, selected = NULL)
                     ),
                     column(4,
                            checkboxGroupInput("AttackType", "Type of Attack", AttackTypeFilter,
                                  selected = NULL),
                            h3("Show in % or counts"),
                            h4(checkboxInput("AbsoluteNos", "View in Count Format", value = FALSE))
                           )
                        )
                           )
                        ),
             tabItem(tabName = "BarChart",
                     fluidPage(

                       plotOutput('bar'),

                       hr(),
                       fluidRow(
                         column(3,
                                # h4("Select up to four points"),
                                checkboxGroupInput("INTS.Bar", "Internationally Motivated",
                                                   International,
                                                   selected = NULL),
                                checkboxGroupInput("TargetTypes.Bar", "Target", TargetChoices, selected = c("tgt.abortion", 
                                                                                                            "tgt.business", 
                                                                                                            "tgt.citizens")),
                                checkboxInput("AllTargets", "Select All Targets", value = FALSE)
                         ),
                         column(4, offset = 1,
                                checkboxGroupInput("WeaponType.Bar", "Type of Weapon Used",
                                                   WeaponFilters, selected = NULL),
                                checkboxInput("AllWeapons", "Select All Weapons", value = FALSE)
                         ),
                         column(4,
                                checkboxGroupInput("AttackType.Bar", "Type of Attack", AttackTypeFilter,
                                                   selected = NULL),
                                checkboxInput("AllAttacks", "Select All Types of Attack", value = FALSE)
                         )
                       )
                     )
             ),
           tabItem(tabName = "ComparisonChart",
                   fluidPage(

                     plotOutput('comparison'),

                     hr(),
                     fluidRow(
                       column(3,
                              h3("Select up to five countries to compare"),
                              selectizeInput("C1", "Country 1", country.set, selected = "United States"),
                              selectizeInput("C2", "Country 2", country.set, selected = "United Kingdom"),
                              selectizeInput("C3", "Country 3", country.set, selected = "India"),
                              selectizeInput("C4", "Country 4", country.set, selected = "China"),
                              selectizeInput("C5", "Country 5", country.set, selected = "Iraq")
                       ),
                       column(8, offset = 1,
                              h3("Select a variable to compare"),
                              selectInput("SeriesSelect", "Select Series to Compare", radioChoices, selected = "wp.explosives")
                       )
                     )
                   )
                   ),
               tabItem(tabName = "map",
                       fluidRow(leafletOutput("map_leaflet", height = 700, width = 1000))

               )
           )
  )
))


