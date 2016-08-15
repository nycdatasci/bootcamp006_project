
library(shinydashboard)
library(googleVis)
library(DT)
library(leaflet)
require(googleVis)


shinyUI(dashboardPage(
  dashboardHeader(title = "Trulia"),
  
  #----------------------------------------SideBar-------------------------------------
  
  dashboardSidebar(
    tags$head(
       tags$style(HTML('.shiny-server-account { display: none; }'))
      ),
    
    # sidebarUserPanel("Trulia"
    #   # image = "https://course_report_production.s3.amazonaws.com/rich/rich_files/rich_files/567/s300/data-science-logos-final.jpg"
    #   ),
    
    
                   #----------------------------SidebarMenu -------------------------------------------------
    
    sidebarMenu(
      menuItem("Introduction", tabName = "Intro", icon = icon("comment")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Neighbourhood ", tabName = "plots1", icon = icon("database")),
      menuItem("Crime Rating", tabName = "plots2", icon = icon("database")),
      menuItem("School Ratings", tabName = "plots3", icon = icon("database"))
      # menuItem("Graphs", tabName = "graphs", icon = icon("comment"))
      )
    
                   
  ),
  
  #--------------------------------------------Body------------------------------------
  
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "CSS/style.css")
    ),
    
    
    
    tabItems(
      tabItem(tabName = "map",
              
              div(
                class = "outer",
               
                  
                
                leafletOutput("map"),  height = 800 , width = "100%"),
              
                absolutePanel(
                  id = "hover_box",
                  class = "panel panel-default",
                  fixed = FALSE,
                  draggable = TRUE,
                  top = 20,
                  left = "auto",
                  right = 20,
                  bottom = "auto",
                  width = 330,
                  height = "auto",
                  # div(align = 'center', h3('New York Rentals')),
                  
                  
                  checkboxGroupInput(inputId ="Neighborhoods", label = h3(" Manhattan Neighborhood"), 
                                     choices = unique(as.character(IndividualApartment2$Neighborhood)),
                                     selected = IndividualApartment2$Neighborhood, 
                                     width = "400px"
                                     ),

                  
                  selectInput(inputId = "Bedrooms", label= "Number of Bedrooms", 
                              choices =  c("All" ,unique(sort(IndividualApartment2$NoOfBedroom,
                                                     decreasing = FALSE))),
                              selected = "All", selectize = FALSE),
                  
                  selectInput(inputId = "Bathrooms", label ="Number of Bathrooms", 
                              choices =c("All",unique(sort(IndividualApartment2$NoOfBathRoom,
                                          decreasing =FALSE))),
                              selected = "All", selectize = FALSE),
                              
                  # selectInput(inputId ="CrimeLevels", label ="Crime Level",
                  #             choices = unique(as.character(IndividualApartment2$CrimeLevel)),
                  #             selected = "Low", selectize = FALSE),
                  # 
                  div(class="row-fluid",
                      div(class="field1",numericInput("Rentmin", label = "Rent-min", value = 500)), 
                      div(class="field2",numericInput("Rentmax", label = "Rent-max", value = 1500000))
                  ),
                  
                  
                  
                    plotOutput("barplot1", height = 180)
                  
                
              )), 
              
                       
      
      tabItem(tabName = "data",
              fluidRow(box(DT::dataTableOutput("table"),width = 12))),
      
      
      tabItem(tabName = "plots1",
              
              fluidRow(box( htmlOutput("plot2"), height = 300, width = 1180 )),
              
              fluidRow(box( htmlOutput("plot3"), height = 300 , width = 1180))
              
      ),
      
      tabItem(tabName = "plots2",
              
              fluidRow(box( htmlOutput("plot4"), height = 500, width = 1180 ))
              
              # fluidRow(box( htmlOutput("plotx"), height = 300 , width = 1180))
              
      ),
      
      tabItem(tabName = "plots3",
              
              fluidRow(box( htmlOutput("plot5"), height = 200, width = 1180 )),
              
              fluidRow(box( htmlOutput("plot6"), height = 200 , width = 1180)),
              
              fluidRow(box( htmlOutput("plot7"), height = 200 , width = 1180))
              
      ),
      

    
      
      tabItem(tabName = "Intro",
              tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "CSS/style.css")
              ),
            
              div(id = 'intro',
                  
                  fluidRow(column(
                    12,
                    div(class = "imageContainer",
                        img(src="images/Apt1.png", height = 270, width = 400)
                    )
                    # div(class = "imageContainer",
                    #     img(src="images/Apt2.png", height = 300, width = 400)
                    # )
                    
                  )),
                  
                  fluidRow(column(
                    12,
                    
                    h1('Trulia Apartment Listings in Manhattan'),
                    
                    h4('Trulia is an online residential real estate site for home buyers, sellers, renters and real estate professionals in the United States. It lists properties for sale and rent as well as tools and information needed to be successful in the home search process '),
                    h4(
                      'This app facilitates the user to search for rental apartments in Manhattan according to their need and budget.'),
                    
                    h4(
                      'User can visualize the apartments based on spatial location, price and apartment type. They can look at the apartments based on number of bedrooms and bathrooms.'
                    ),
                    
                    h4(
                      'The filtered apartments are displayed on the map and more information about  the apartment can be accessed by clicking on the point on the map  which displays additional information on the  pop-up box.'
                    ),
                    h4(
                      'The app also provides information about apartments in different neighborhood which are safer to live in based on crime rate which has been categorized into 3 groups.'
                    ),
                    h4(
                      'It also provides information about the school ratings near the apartments. The ratings have been provided for Elementary school, Middle school and High school. '
                    ),
                    h4(
                      'The data for this app was downloaded by scraping the "Truiia.com" website. Only apartments located in the Manhattan have been focused at the moment due to time constraints. This app will cover other areas of New York in near future '
                    ),
                    h4(
                      'Enjoy the App'
                    )
                  ))
                  
                  
               
                  
                  
                  
                  
                  ))
    
    
    )
  )
))