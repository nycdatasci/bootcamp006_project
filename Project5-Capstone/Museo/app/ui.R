library(shiny)
library(leaflet)
library(rjson)
library(tools)

shinyUI(fluidPage(theme = "bootstrap.css",
                  
                  tags$style('#controls {background-color: #484848; opacity : .8; color: white}'),
                  tags$style("#reminder { width:100%; margin-top: 50px;}"),
                  tags$style('nav .container:first-child {margin-left:10px; width: 100%;}'),
                  tags$div(tags$style('#intro { background-image: url("E_tower.jpg");
                                        opacity: .85; height: 800px; width: 100%; color:white;}')),
                  
                  navbarPage("Museums Finder(Beta)", id='nav',
                             tabPanel('Get Recommendation',
                                      fluidPage(
                                          fluidRow(
                                              column(3, 
                                                     selectizeInput("selected_museum", 
                                                                    h5("1. Search or Scroll Down to Select Musuem(s) of Interest:"),
                                                                    # sort(museum$MuseumName),
                                                                    museum$MuseumName, 
                                                                    multiple = TRUE)
                                                     
                                              ),
                                              
                                              column(2, align = "bottom",
                                                     fluidRow(
                                                         helpText(h5("2. Hit the Button to Get Recommendation")),
                                                         actionButton("recommend_btn", h4(">> Recommend <<"))   
                                                     )),
                                              
                                              
                                              column(4, align = "bottom", htmlOutput("reminder"))
                                          ), 
                                          hr(),
                                          fluidRow(
                                              column(2, htmlOutput("name_0")),
                                              column(2, htmlOutput("name_1")),
                                              column(2, htmlOutput("name_2")),
                                              column(2, htmlOutput("name_3")),
                                              column(2, htmlOutput("name_4")),
                                              column(2, htmlOutput("name_5"))
                                              
                                          ),
                                          fluidRow(
                                              # put align="center" after the col width to center the img if needed
                                              column(2, htmlOutput("img_0")),
                                              column(2, htmlOutput("img_1")),
                                              column(2, htmlOutput("img_2")),
                                              column(2, htmlOutput("img_3")),
                                              column(2, htmlOutput("img_4")),
                                              column(2, htmlOutput("img_5"))
                                          ),
                                          fluidRow(
                                              column(2, htmlOutput("content_0")),
                                              column(2, htmlOutput("content_1")),
                                              column(2, htmlOutput("content_2")),
                                              column(2, htmlOutput("content_3")),
                                              column(2, htmlOutput("content_4")),
                                              column(2, htmlOutput("content_5"))
                                          ),
                                          fluidRow(
                                              column(2, htmlOutput("common_0")),
                                              column(2, htmlOutput("common_1")),
                                              column(2, htmlOutput("common_2")),
                                              column(2, htmlOutput("common_3")),
                                              column(2, htmlOutput("common_4")),
                                              column(2, htmlOutput("common_5"))   
                                          ),
                                          fluidRow(
                                              column(2, htmlOutput("add_0")),
                                              column(2, htmlOutput("add_1")),
                                              column(2, htmlOutput("add_2")),
                                              column(2, htmlOutput("add_3")),
                                              column(2, htmlOutput("add_4")),
                                              column(2, htmlOutput("add_5"))
                                          ),
                                          fluidRow(
                                              column(2, htmlOutput("des_0")),
                                              column(2, htmlOutput("des_1")),
                                              column(2, htmlOutput("des_2")),
                                              column(2, htmlOutput("des_3")),
                                              column(2, htmlOutput("des_4")),
                                              column(2, htmlOutput("des_5"))
                                          )
                                          
                                      )
                                      
                                      
                             ),
                             tabPanel("Museum Map",
                                      fluidPage(div(class="outer", 
                                                    leafletOutput("map", width = "100%", height = 1000),
                                                    absolutePanel(h3("Museum Filter"),id = "controls", 
                                                                  class = "panel", fixed = TRUE, draggable = TRUE, 
                                                                  top = 80, left = 100, right = "auto", bottom = "auto",
                                                                  width = 180, height = "auto",
                                                                  selectInput('museum_type', h4('Museum Types:'), 
                                                                              c("History Museums" ,"Military Museums" ,"Points of Interest & Landmarks",
                                                                                "Natural History Museums", "Science Museums", "Art Museums", "Sights & Landmarks",
                                                                                "Specialty Museums", "Art Galleries", "Children's Museums", "Historic Sites"),
                                                                              multiple = TRUE, selectize = TRUE),
                                                                  
                                                                  checkboxGroupInput("good_for", h4("Good For:"),
                                                                                     c("All Ages" = "all.ages", 
                                                                                       "Family" = "whole.family",
                                                                                       "Children" = "great.for.kids",
                                                                                       "Rainy Day"="rainy.day"
                                                                                     ), selected = c()),
                                                                  checkboxGroupInput("visit_length", h4("Suggested Length of Visit:"),
                                                                                     c("<1 hour" = "<1 hour ",
                                                                                       "1-2 hours" = "1-2 hours ",
                                                                                       "2-3 hours" = "2-3 hours ",      
                                                                                       ">3 hours" = "More than 3 hours "),
                                                                                     selected = c()),
                                                                  checkboxGroupInput("other_option", h4("Other Options:"),
                                                                                     c("Free Admission"="free.admission", 
                                                                                       "Has Gift Shop"="gift.shop", 
                                                                                       "Has Audio guide"="audio.guide"),
                                                                                     selected = c()),
                                                                  actionButton("go", "Search")
                                                    )))
                             ),
                             
                             
                             tabPanel("About This App",
                                      div(id = 'intro',
                                          fluidPage(
                                              h3("Motivation"),
                                              div("I love traveling and visiting museums. Sometimes it took time to find the right museums."),
                                              div("The app is for museum lovers to discover the museums that might match one's preference."),
                                              br(),
                                              h3("Data"),
                                              p("Data of roughly 1600 museums were collected from TripAdvisor. Click", a(href="https://github.com/annecool37/MuseumRecommendation","here"), "to see the code."),
                                              br(),
                                              h3("Author"),
                                              div("Anne Chen", a(href="https://www.linkedin.com/in/chiaanchen","LinkedIn")),
                                              div("View other projects at",
                                                  a(href="http://blog.nycdatascience.com/author/annecool37/","Blog"),
                                                  " or ", a(href="https://github.com/annecool37","Github"), ".")
                                              
                                          )   
                                      )        
                             )
                             
                  )
)
)