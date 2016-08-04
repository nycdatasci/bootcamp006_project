load("~/Downloads/violations_2016.RData")

violations = violations[as.numeric(violations$month)>5,]
violations$popup = paste(violations$Description, violations$Make, violations$Model, sep = ", ")
violations$speed = grepl("SPEED", violations$popup)
violations$alc = c(grepl("ALCOHOL", violations$popup) | 
                         grepl("INTOXICATED", violations$popup) |
                         grepl("INFLUENCE", violations$popup))

library(dplyr)
library(shiny)
library(ggplot2)
library(shinydashboard)
library(leaflet)
library(maps)
library(ggmap)
library(rgdal)

ui = dashboardPage(header, sidebar, body)

header = dashboardHeader()

sidebar = dashboardSidebar(width = 320,
                           sliderInput(inputId = "mday_range", "Days of the Month",
                                                   value = c(1,31),
                                                   min = 1, 
                                                   max = 31, 
                                                   step = 1, 
                                                   ticks = 31),
                           sliderInput(inputId = "hour_range", "Hour Range",
                                       value = c(0,23),
                                       min = 0, 
                                       max = 23, 
                                       step = 1, 
                                       ticks = 24),
                           selectInput(inputId = "wday_range", 
                                       label = "Days of the Week", 
                                       choices = c("Sunday",
                                                   "Monday",
                                                   "Tuesday",
                                                   "Wednesday",
                                                   "Thursday",
                                                   "Friday",
                                                   "Saturday"),
                                       multiple = T,
                                       selected = c("Sunday",
                                                    "Monday",
                                                    "Tuesday",
                                                    "Wednesday",
                                                    "Thursday",
                                                    "Friday",
                                                    "Saturday")),
                           selectInput(inputId = "gen", label = "Gender", 
                                       choices = c("Do not filter", 
                                                   "Male" = "M", 
                                                   "Female" = "F", 
                                                   "Unidentified" = "U")),
                           selectInput(inputId = "race", "Race", 
                                       choices = c("Asian" = "ASIAN", 
                                                   "Black" = "BLACK",
                                                   "Hispanic" = "HISPANIC",
                                                   "Native American" = "NATIVE AMERICAN",
                                                   "White" = "WHITE",
                                                   "Other" = "OTHER"),
                                       multiple = TRUE,
                                       selected = levels(violations$Race)),
                           selectInput(inputId = "Alc", "Alcohol", 
                                       choices = c("Do not filter", "No" = FALSE,"Yes" = TRUE)),
                        selectInput(inputId = "speed", "Speeding", 
                                    choices = c("Do not filter", 
                                    "No" = FALSE,
                                    "Yes" = TRUE)))


body = dashboardBody(
      textOutput("number"),
      textOutput("number_red"),
      leafletOutput("map", width = "100%", height = 600),
      box(title = "Color", selectInput(inputId = "green", label ="Red", 
                                       choices = c("Nothing",
                                                   "WHITE",
                                                   "BLACK",
                                                   "HISPANIC",
                                                   "NATIVE AMERICAN",
                                                   "seatbelts: yes" = "Yes", 
                                                   "seatbelts: no" = "No", 
                                                   "Male" = "M",
                                                   "Female" = "F",
                                                   "Speeding",
                                                   "Alcohol"),
                                       multiple = T, selected = "Nothing"),
                                    
collapsible = T))

server <- shinyServer(function(input, output) {

      data = reactive({violations = violations[c( 
                                                 violations$hour >= input$hour_range[1]
                                                 & violations$hour <= input$hour_range[2]
                                                 & violations$day_week %in% input$wday_range 
                                                 & violations$day_of_month>=input$mday_range[1]
                                                 & violations$day_of_month<=input$mday_range[2]
                                                 & violations$Race%in%input$race
                                                 ),]
                        if (input$Alc != "Do not filter"){
                              violations = violations[violations$alc%in%input$Alc, ]
                        }
                        if (input$gen != "Do not filter"){
                              violations = violations[violations$Gender%in%input$gen, ]
                        }
                        if (input$speed != "Do not filter"){
                               violations = violations[violations$speed%in%input$speed, ]}
                        return(violations)
                        })
      
      green = reactive({a = data()
      if("Nothing"%in%input$green==F){
            if("Yes" %in% input$green | "No" %in% input$green){
            a = a[a$Belts%in%input$green,]}
            if("M" %in% input$green | "F" %in% input$green) {
                  a=a[a$Gender %in% input$green,]}
            if("WHITE" %in% input$green |
               "NATIVE AMERICAN"%in% input$green |
               "BLACK"%in% input$green |
               "HISPANIC"%in% input$green){
                  a=a[a$Race %in% input$green,]}
            if("Speeding"%in%input$green){
                  a=a[a$speed==T,]}
            if("Alcohol"%in%input$green){
                  a=a[a$alc==T,]}
            }
      else{a=a[a$Latitude==max(a$Latitude),]}
      return(a)
            })
      
      
      output$number = renderText({paste("Number of Violations:", nrow(data()))})
      output$number_red = renderText({paste("Number of Red Violations:", nrow(green()))})
      output$map = renderLeaflet({
            m = leaflet() %>% 
                  addTiles() %>%
            addCircles(data = data(),
                                 lat = data()$Latitude,
                                 lng = data()$Longitude,
                                 fillColor = "blue",
                                 color = "blue",
                                 fillOpacity = .5,
                                 popup = data()$popup,
                                  radius = 100)%>%
            addCircles(data = green(),
                                   lat = green()$Latitude,
                                   lng = green()$Longitude,
                                   fillColor = "red",
                                   color = "red",
                                   fillOpacity = .5,
                                   popup = green()$popup,
                                   radius = 105)
            
            
            

            })
      })

ui = dashboardPage(header, sidebar, body)

shinyApp(ui = ui, server = server)






