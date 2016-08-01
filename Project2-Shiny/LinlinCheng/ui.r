#install.packages('shinydashboard')

library(shiny)
library(shinydashboard)
library(dygraphs)
library(shinyGlobe)
library(rglwidget)
library(plotly)

shinyUI(fluidPage(theme="bootstrap.css",
  titlePanel('Consumption, Production, and Global Warming'),
  dashboardPage(skin = "blue",
  dashboardHeader(title = 'Dashboard'),
  dashboardSidebar(
    sidebarUserPanel("Linlin Cheng", image = 'https://lh3.googleusercontent.com/-Zfhhozo1zno/UkD7WsSdckI/AAAAAAAAAB8/RsWhE5MYSBYZmbWVNg10Z4t8s8sj4993ACEw/w280-h278-p/DSC02503.JPG'),
    sidebarMenu(
      menuItem("Background Info", tabName = "background", icon = icon("file")),
      menuItem("Carbon Emission by Country", tabName = "country", icon = icon("map")),
      menuItem("Production and Consumption", tabName = "growth", icon = icon("line-chart")),
      menuItem("What we can do", tabName = "conclusion", icon = icon("list")),
      menuItem("Source", tabName = "source", icon = icon("angle-double-down")),
      menuItem("About Me", tabName = "me", icon = icon("database"))
      
      )
  ),
  
  
  #########################
  dashboardBody(
    tabItems(
      tabItem(tabName = "background",
                fluidRow(
                        dygraphOutput("timeplot"),
                        helpText("Data source: Lawrence Berkeley National Laboratory"),
                        helpText("Temperatures are in Celsius and reported as anomalies relative to the Jan 1951-Dec 1980 
                                  average. "),

                        br(),
                        globeOutput('globe'),
                        helpText("Data source: NASA Earth Observations."),
                        helpText("Land surface temperature is how hot or cold the ground feels to the touch. An anomaly is when something is different from average. 
                                 These maps show where Earth’s surface was warmer, as in red, or cooler as in blue, in the daytime than the average temperatures for the same week or month from 2001-2010. 
                                 This plot takes the data for the week of Jun, 17, 2016.")
                        )

              ),


      ##############
      tabItem(tabName = "country",

              fluidRow(
                column(width = 9,
                       box(width = NULL, plotOutput("worldmap"), collapsible = FALSE,
                           title = "World Results", status = "primary", solidHeader = TRUE),
                        h4("Typically,	 emissions	 statistics	 are	 compiled	 according	 to	 production‐based:
                            measuring	 emissions	 occurring	 within	 sovereign	 borders."),
                        h4("Consumption based statistics includes the distribution across economies of 
                           final consumption of embodied carbon that has been emitted anywhere in the world along global production chains")
                        ),
                
                column(width = 3,
                       box(width = NULL, status = "primary", solidHeader = TRUE, title = "Parameters",
                           uiOutput("yearSlider"),
                           selectInput("wquestion", label = "Question:",
                                       choices = list(
                                         #"on Gross Level"=c(
                                         #"CO2 embodied in domestic final demand",
                                         #"CO2 emissions based on production",
                                         #"Net-exports of CO2 emissions"),
                                         "on Per Capita Level"=c(
                                         "CO2 embodied in final demand per capita",
                                         "CO2 emissions per capita based on production"))

                                       ),
                                       selected = "CO2 embodied in final demand per capita"
                                     
                           ),
                           box(width = NULL, status = "warning",
                               h5("Data source: OECD Stats."))
                               ))
                       ),
      ##############



      tabItem(tabName = "growth",

              fluidRow(

                column(width = 9,
                       box(width = NULL, plotlyOutput("scatterplot"), collapsible = FALSE,
                           title = "Scatter Plot of Carbon Emission Vs. Income",
                           status = "primary", solidHeader = TRUE),
                       h4("Final demand based Carbon statistics corresponds better with National Income, 
                          a proxy of disposable income and consumptions")
                ),

                column(width = 3,
                       box(width = NULL, status = "primary", solidHeader = TRUE, title = "Parameters",
                           selectInput("carboninput", label = "Carbon Index:",
                                       choices = list(
                                         #"on Gross Level"=c(
                                         #"CO2 embodied in domestic final demand",
                                         #"CO2 emissions based on production",
                                         #"Net-exports of CO2 emissions"),
                                         #"on Per Capita Level"=c(
                                           "CO2 embodied in final demand per capita",
                                           "CO2 emissions per capita based on production")

                           ),
                           selected = "CO2 embodied in final demand per capita"
                       ),


                       box(width = NULL, status = "primary", solidHeader = TRUE, title = "Parameters",
                           selectInput("gdpinput", label = "GDP Index",
                                       choices = list(
                                         "GDP",
                                         "GDP_Per_Capita"
                           ),
                           selected = "GDP_Per_Capita"
                       )

              )
                )
              )
      ),

      tabItem(tabName = "conclusion",
              img(src = "https://developmentwatchuk.files.wordpress.com/2011/02/when-sea-levels-attack.jpg",height = 500, width = 720),
              helpText("Image Source: Development Watch UK"),
              br(),
              h3("Yes, the global average temperature is rising, and so is the sea level. 
                 But there are still many things we can do to help slow down the process:"),
              h4("1. Use Water/Power Efficiently"),
              h4("2. Reduce, Reuse, and Recyle"),
              h4("3. Drive smart and purchase Fuel Efficient Vechiles"),
              h4("4. SPREAD THE WORD"),
              h4("..."),
              br(),
              tags$a(href="https://www3.epa.gov/carbon-footprint-calculator/", "You can also find out about your own Carbon Footprint HERE")
              
              
            ),

      tabItem(tabName = "source",

              tabItem(tabName = "about",
                      fluidRow(
                        box(title = "Source", status = "info", solidHeader = TRUE, collapsible = TRUE, width = 8,
                            h4("Click here for the data source:"),
                            tags$a(href="www.berkeleyearth.lbl.gov/auto/Global/Complete_TAVG_complete.txt", "Estimated Global Land-Surface Temperature"),
                            br(),
                            tags$a(href="stats.oecd.org", "Carbon Emission Data"),
                            br(),
                            tags$a(href="data.worldbank.org", "GDP Data"),
                            br(),
                            br(),
                            h4("Click here for image source:"),
                            tags$a(href="https://developmentwatchuk.wordpress.com/2011/02/18/where-should-you-live-to-avoid-global-warming/", "Sea Level Rising Schedule"),
                            br(),
                            tags$a(href="http://neo.sci.gsfc.nasa.gov/view.php?datasetId=MOD_LSTAD_E&date=2016-06-01", "Temperature Anormalies"),
                            br(),
                            br(),
                            h4("Click here for text source:"),
                            tags$a(href="https://www3.epa.gov/climatechange/science/indicators/ghg/", "Global Warming text"),
                            br(),
                            tags$a(href="http://www.oecd.org/sti/ind/EmbodiedCO2_Flyer.pdf", "Carbon Emission by Production and Consumption text")
                            )
                      ))


              ),

      tabItem(tabName = "me",

             fluidRow(

             img(src = "https://lh3.googleusercontent.com/-Zfhhozo1zno/UkD7WsSdckI/AAAAAAAAAB8/RsWhE5MYSBYZmbWVNg10Z4t8s8sj4993ACEw/w280-h278-p/DSC02503.JPG",height = 250, width = 250),
             br(),
             br(),

             h4(      "Linlin is an explorer and fast learner, both in and outside of the classroom.
              With a background in quantitative Economics, she well trained to identify patterns
                        using rigorous statistical methods and justifying them with proper theories.
                        Prior to joining the bootcamp, Linlin obtained her BA/MA from New York University
                        and received computational training in the PhD level at the University of Maryland.
                        She has ample experience conducting modeling in both applied and theoretical
                        settings , such as in Market Competition Analysis, Financial Economics, and Environmental
                        Regulatory policies.  She also has experience working with UNDESA as quantitative assistant."
             ),

             br(),
             tags$span(
               tags$a(href = "https://www.linkedin.com/in/linlin-lynette-cheng-41422a87?trk=hp-identity-name", icon("linkedin", "fa-2x")),
               tags$a(href = "https://plus.google.com/u/0/+LinlinCheng/posts", icon("google-plus", "fa-2x"), style = "margin-left: 20px;")
             )
             )

)
))
))
)



