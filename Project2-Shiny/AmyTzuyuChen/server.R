library(shiny)
library(leaflet)
library(dplyr)
library(googleVis)
library(DT)

shinyServer(function(input, output) {
  mapInput <- eventReactive(input$go,{
    newkid<-kid[kid$Borough %in% input$borough & 
                kid$Type %in% input$type & 
                kid$Length %in% input$length,]
    newkid
    })
  
  output$map <- renderLeaflet({
    leaflet(kid) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.951801,40.702988, zoom = 11) %>%
      addCircleMarkers(~lon, ~lat, radius=~Seats/3, clusterOptions = markerClusterOptions(),
                        fillColor=~pal(Type),fillOpacity=0.7, stroke=FALSE,
                        popup=~paste('<b><font color="Green">',Name,'</font></b><br/>','Address:',Address,'<br/>','Seats:',Seats,'<br/>','Tel:',
                                     Phone,'<br/>','Email:',Email,'<br/>','URL: <a target="_blank" href=', Website, ">Offical Website</a>")) %>%
       addLegend("bottomright", pal = pal, values = ~kid$Type,
                 title = "Pre-K Type", opacity = 0.8)
      })
    
    observe({
      leafletProxy("map", data = mapInput()) %>%
      clearMarkerClusters() %>%
      addCircleMarkers(~lon, ~lat,radius=~Seats/3, clusterOptions = markerClusterOptions(),
                       fillColor=~pal(Type),fillOpacity=0.7,stroke=FALSE, 
                       popup=~paste('<b><font color="Green">',Name,'</font></b><br/>','Address:',Address,'<br/>','Seats:',Seats,'<br/>','Tel:',
                                    Phone,'<br/>','Email:',Email,'<br/>','URL: <a target="_blank" href=', Website, ">Offical Website</a>"))})
  
  
  output$table <- renderDataTable(kidtable,
                                  options=list(aLengthMenu = c(15,25,50), 
                                               iDisplayLength = 15,
                                               columnDefs = list(list(width = '160px', targets = c(10,11)),
                                                                 list(width = '120px', targets = c(4,5)),
                                                                 list(width = '200px', targets = 3))))
                      
  barInput <- reactive({
    newboroughkid<-boroughkid[,c("Borough",input$yvar)]
    newboroughkid<-newboroughkid[newboroughkid$Borough %in% input$boroughbar,]
    newboroughkid
  })
  
  output$bar <- renderGvis({
    gvisColumnChart(barInput(),options = list(title="Are There Enough Seats at Pre-Kindergartens",hAxes="[{title:'Borough',
                                             titleTextStyle: {color:'blue'}}]"
                                              , vAxes="[{title:'Number of Eligible Kids', 
                                              titleTextStyle: {color: 'blue'}}]",
                                              width=800,height=500))
  })
  
  
})