library(shiny)
library(shinydashboard)
library(DT)
library(plotly)


shinyUI(dashboardPage(skin = "blue",
                      
                      
    dashboardHeader(title = "H-1B VISA Explorer",
                    disable = FALSE),
    
    dashboardSidebar(
      
        sidebarUserPanel("Deepak Khurana",
                         image = "Deepak_Khurana.jpg"),
        
        sidebarMenu(
            menuItem("About",
                     tabName = "about",
                     icon = icon("info")
                     ),
            menuItem("Temporal Analysis",
                     tabName = "timeseries",
                     icon = icon("expand"),
                     
                    menuItem("Time Series",
                              tabName = "timeseries",
                              icon = icon("line-chart")
                     ),
                    menuItem("Processing Time",
                             tabName = "PT",
                             icon = icon("bar-chart")
                             
                    )
            ),

            menuItem("Statewise Analysis",
                     tabName = "visual",
                     icon = icon("expand"),
            
                     selectizeInput("selected1",
                                    "Select the list and visualisation",
                                    choice1),       
            
            menuItem("Map",
                     tabName = "map",
                     icon = icon("map")
                     
                     
            ),
            menuItem("Bar-Chart",
                     tabName = "Bar",
                     icon = icon("bar-chart ")
            ),
            menuItem("Pie Chart",
                     tabName = "pie",
                     icon = icon("pie-chart")
            ),
            menuItem("Histogram",
                     tabName = "Histogram",
                     icon = icon("th")
            ),
            menuItem("Data Table",
                     tabName = "data",
                     icon = icon("table ")
              )
            ),

            menuItem("Top of the bunch",
                     tabName = "criteria",
                     icon = icon("expand"),
                     
                     selectizeInput("selected2",
                                    "Select the list and criteria",
                                    choice2
                     ),
                     
            
            menuItem("Applications Approved",
                     tabName = "cert",
                     icon = icon("flag-checkered ")
            ),
            menuItem("Average Salary",
                     tabName = "salary",
                     icon = icon("dollar")
            ),
            sliderInput("slider2", "Select how many items from the top of the list to display", 3, 20, 10)
            
            ),
            #menuItem("Summary",
             #        tabName = "summary",
              #       icon = icon("cog")
               #      ),
            
            #menuItem("Source Code",
            #         tabName = "code",
            #         icon = icon("code")
            #         ),    
            
            menuItem("Author",
                     tabName = "author",
                     icon = icon("user")
                     )
        )

    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "about",
                    fluidRow(
                      box(
                        title = "Background", solidHeader = TRUE,status="primary", width=12, height = "550px",
                        strong("H-1B Visa"),
                        br(),
                        p(
                          "The US H1B visa is a non-immigrant visa that allows US companies to employ foreign workers 
                          in specialty occupations that require theoretical or technical expertise in specialized fields 
                          such as in architecture, engineering, mathematics, science, and medicine. The job must meet certain specific
                          criteria to qualify as a specialty occupation. Under the visa a US 
                          company can employ a foreign worker for up to six years."
                        ),  
                        p(
                          "Individuals are not able to apply for an H1B visa to allow them to work in the US. The employer
                          must petition for entry of the employee. H1B visas are subject to annual numerical limits.Current immigration 
                          law allows for a total of 85,000 new H-1B visas to be made available each government fiscal year. This number 
                          includes 65,000 new H-1B visas issued for overseas workers in professional or specialty occupation positions, 
                          and an additional 20,000 visas available for those with an advanced degree from a US academic institution.For details you can follow the link below"
                        ),
                   
                        tags$a(href = "https://www.uscis.gov/eir/visa-guide/h-1b-specialty-occupation/understanding-h-1b-requirements", "U.S. Citizenship and Immigration Services (USCIS)"),
                        
                        
                        tags$hr(),   
                        
                        strong("Application"),
                        br(),
                        br(),
                        p(
                          "H-1B VISA Explorer application visualises and facilitates the exploration of data made available by Office of Foreign Labor Certification. 
                          How long does it take for an H-1B application to be processed from the date of submission ? When are the most application decisions made? 
                          What percentage of applications get approved or denied? Which Employers , States , Occupations and Job Titles 
                          file for most applications. What's the average salary for H-1B VISA applicants ? Which profession ,job title or 
                          state offers top salaries? To find out the answers to these and more explore the app. Have fun !  "
                        ), 

                        
                        strong("Data"),
                        br(),
                        p("The case disclosure file covers VISA determinations issued between October 1, 2015 through June 30, 2016.Link to the data is below."),
                        tags$a(href = "https://www.foreignlaborcert.doleta.gov/performancedata.cfm", "Office of Foreign Labor Certification (OFLC) Performance Data")
                      )
                    )
                        
            ),
            tabItem(
                  tabName = "pie",
                  #h2("Pie tab content"),
                  fluidRow(
                    box(htmlOutput("pie"), width = 12, height = 450)
                  )
            ),
            tabItem(
                  tabName = "PT",
                  #h2("Time Series tab content"),
                  fluidRow(
                    box(htmlOutput("column"), width = 12),
                    box(sliderInput("slider1", "Range of days to plot", 0, 180, 14), width = 12)
                  )
            ),
            tabItem(
              tabName = "timeseries",
              #h2("Time Series tab content"),
              fluidRow(
                box(dygraphOutput("dygraph"), width = 12, height = 550)
              )
            ),
            tabItem(
              tabName = "Histogram",
              #h2("Histogram tab content"),
              fluidRow(
                box(htmlOutput("hist1"), width = 12, height = 450)
              )
            ),
            tabItem(
              tabName = "Bar",
              fluidRow(
                box(htmlOutput("bar", width = 12, height = 600)
                    )
              )
            ),
            tabItem(
              tabName = "map",
              fluidRow(
                infoBoxOutput("avgBox", width = 5),
                infoBoxOutput("maxBox", width = 4),
                infoBoxOutput("minBox", width = 3)
                
                      ),
              fluidRow(
                box(htmlOutput("map"), height = 350 , width = 12)
                      )
            ),
            tabItem(
              tabName = "summary",
              h2("Summary tab content")
            ),
            tabItem(
              tabName = "cert",
              #h2("Pie tab content"),
              fluidRow(
                box(htmlOutput("pie1"), height = 600 , width = 12)
               )
              ),
            tabItem(
              tabName = "salary",
              #h2("Pie tab content"),
              fluidRow(
                box(htmlOutput("pie2"), height = 600 , width = 12)
              )
            ),
            tabItem(
              tabName = "data",
              fluidRow(
                box(DT::dataTableOutput("table"), width = 12)
              )
            ),
            tabItem(
              tabName = "code",
              #h2("Source code for app tab content"),
              tabBox(
                      width=12,
                      #title = "Source code",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "tabset1", height = "550px",
                      tabPanel("global.R",
                               code("code displays your text similar to computer code")
                      ),
                      tabPanel("server.R",
                               code("code displays your text similar to computer code")
                      ),
                      tabPanel("ui.R",
                               code("shinyUI(fluidPage( "),
                               br(),
                               code("))")
                               ),
                      tabPanel("helper.R",
                               code("code displays your text similar to computer code")
                               ),
                      tabPanel("data_preparation.R",
                               code("code displays your text similar to computer code")
                               )
                      
                    )
            ),
            tabItem(tabName = "author",
                    fluidRow(
                        box(
                        title = "About Me", solidHeader = TRUE,status="primary", width=12, height = "550px",
                        p(
                          strong("Deepak Khurana"), "is finishing his PhD in Astrophysics at NYU. 
                          After finishing his Masters in Physics from 'Indian Institute of Technology Kharagpur', 
                          one of the top engineering school in India, he decided to pursue higher education in US. 
                          He was awarded Henry M. Maccracken fellowship at NYU to pursue PhD in Physics. 
                          In graduate school he worked on modeling the Magnetic Field of our Milky Way Galaxy 
                          and developed an expertise in R, Python , data analysis , statistical modeling and optimization. 
                          In his spare time he enjoys biking and exploring regions in and around NYC. He enjoys playing soccer 
                          and field hockey."
                          ),  
                        
                       
                        tags$hr(),   
  
                        strong("Contact Details"),
                        br(),
                        br(),
                        tags$a(href = "deepak.khurana@nyu.edu",icon("envelope"), "Email"),
                        br(),
                        tags$a(href = "https://www.linkedin.com/in/dkhurana1306",icon("linkedin"), "LinkedIn"),
                        br(),
                        tags$a(href = "https://github.com/dkhurana1306",icon("github"), "Github"),
                        br(),
                        tags$a(href = "http://facebook.com/dk1306",icon("facebook-official"), "Facebook"),
                        br(),
                        tags$a(href = "https://www.linkedin.com/in/dkhurana1306",icon("graduation-cap"), "Resume"),
                        br(),
                        tags$a(href = "http://blog.nycdatascience.com/author/dkhurana1306/",icon("pencil-square-o "), "Blog"),
                        tags$hr(),
                        
                        strong("Affiliations"),
                        br(),
                        br(),
                        tags$a(href = "https://nycdatascience.com/", "NYC Data Science Academy"),
                        br(),
                        tags$a(href = "http://physics.as.nyu.edu/page/home", "New York University Department of Physics")

 
                  )
                   
                      
              )
                  
          )
        )
    )
))


