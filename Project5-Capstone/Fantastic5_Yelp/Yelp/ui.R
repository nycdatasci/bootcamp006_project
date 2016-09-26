

shinyUI(dashboardPage(
  dashboardHeader(title = "YELP QUEST"),
  
 dashboardSidebar(
    tags$head(
       tags$style(HTML('.shiny-server-account { display: none; }'))
      ),
    
    sidebarMenu(
      menuItem("Introduction", tabName = "Intro", icon = icon("database")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("LDAInputs",tabName = "LDASelect", icon = icon("bar-chart-o")),
      menuItem("Images",tabName = "Images", icon = icon("bar-chart-o")),
      menuItem("Checkin", icon = icon("bar-chart-o"), tabName = "Checkin",
               menuSubItem("Check-in Day-time", tabName = "daytime", icon = icon("database")),
               menuSubItem("Check-in Day", tabName = "day", icon = icon("database")),
               menuSubItem("Check-in time", tabName = "time", icon = icon("database"))
      )
    )
  ),
  
  
  
  ####################################Body#########################################################3
  
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "CSS/style.css")),
    
    ############################################## map ############################################################3
    tabItems(
      tabItem(tabName = "map",
              div(class = "outer",
                leafletOutput("map"),  height = "500" , width = "100%"),
              
                absolutePanel(
                  id = "hover_box",
                  class = "panel panel-default",
                  fixed = FALSE,
                  draggable = TRUE,
                  top = 40,
                  left = "auto",
                  right = 20,
                  bottom = "auto",
                  width = 300,
                  height = "auto",
                   
                  selectInput(inputId = "RestaurantType", label= "Type of Restaurant",
                              choices =  c("All" ,unique(sort(yelpBusiness1$NewCategory,decreasing = FALSE))),
                              selected = "All", selectize = FALSE),

                  selectInput(inputId = "PriceRange", label ="Price Range",
                              choices =c("All",unique(sort(yelpBusiness1$PriceCategory,decreasing =FALSE))),
                              selected = "All", selectize = FALSE),

                  selectInput(inputId ="RatingType", label ="Restaurant Rating",
                              choices = c("All", unique(sort(yelpBusiness1$stars_of_business,decreasing = FALSE))),
                              selected = "All", selectize = FALSE),
                  
                  plotOutput("barplot1", height = 180)
              )), 
      
      ####################################################### Table ############################################
      tabItem(tabName = "data",
              fluidRow(box(DT::dataTableOutput("table"),width = 12))),
      
      ############################################3 LDA Select ####################################################
      
      tabItem(tabName = "LDASelect",
              selectInput(inputId ="LDAid", label ="Choose Restaurant Type",
                                        choices = c("American" = 1,
                                                    "Chinese" = 2,
                                                    "Italian" = 3,
                                                    "Thai" = 4,
                                                    "Mexican" = 5  ),
                                        selected = 1,selectize=FALSE),
              
              fluidRow(box(visOutput('myChartLDASelect'),width = 12))),
      
      #########################################3 images ######################################################3333
      
      tabItem(tabName = "Images",
              selectInput(inputId ="Imageid", label ="Choose Restaurant Type",
                          choices = c("American" = 1,
                                      "Chinese" = 2,
                                      "Italian" = 3,
                                      "Thai" = 4,
                                      "Mexican" = 5  ),
                          selected = 1,selectize=FALSE),
              
              fluidRow(box(uiOutput('ImageSelect'),width = 12))),
      
      ########################################checking time ###################################################
      
      tabItem(tabName = "daytime",
              fluidRow(box(ggvisOutput('plotdaytime'),width = 12))),
      
      tabItem(tabName = "day",
              fluidRow(box(htmlOutput('plotdays'),width = 12))),
      
      tabItem(tabName = "time",
              fluidRow(box(htmlOutput('plothour'),width = 12))),
      
      ###########################################3 Introduction ################################################
      
      tabItem(tabName = "Intro",
              tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "CSS/style.css")
              ),
              div(id = 'intro',
                  
                  fluidRow(column(
                    6,
                    div(class = "imageContainer",
                        img(src="img/yelp.png", height = 400, width = 450)
                    )
                  )
                  ),
                  
                  fluidRow(column(
                    12,
                    h1('Introduction'),
                    
                    h4('Welcome to YelpQuest! YelpQuest is a web application that helps you, a small business owner, grow and compete in your specific market. How so? Using various machine learning algorithms, we have given ways to optimize the location of your restaurant, understand weaknesses in your current market, and Give us a try! On the left hand menu, you will see: '),
                    
                    h4(
                      '1.    LDA Topic Models - By identifying the category of your restaurant, you can see favorite dishes as well as weaknesses.'),
                    
                    h4(
                      '2.    Map - If you are just starting off, select this feature to understand how well you are doing relative to your area. Also, you may use this to choose a good location for your new restaurant.'
                    ),
                    
                    h4(
                      '3.    Image Gallery - Filter the most popular dishes talked among positive yelp reviews'
                    ),
                    h4(
                      'Enjoy the App'
                    )
                  ))
              ))
    )
  )
))