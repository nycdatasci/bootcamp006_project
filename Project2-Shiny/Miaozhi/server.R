library('ggmap')
library('mapproj')
library('leaflet')
library('shiny')
library('ggplot2')
# source('global.R')
library(dygraphs)
library(dplyr)
library(tidyr)

#outputs  
shinyServer(function(input,output){
  output$map <- renderLeaflet({

      df = data.frame(lon = final[final$BEDS == input$select & 
                                            final$BATHS ==input$select2 & 
                                    final$area %in% input$area &
                                    final$NEIGHBORHOOD %in% input$neighbour &
                                    final$cost<input$range,]$lon,
                      lat = final[final$BEDS == input$select & 
                                    final$BATHS ==input$select2 &
                                    final$area %in% input$area &
                                    final$NEIGHBORHOOD %in% input$neighbour &
                                    final$cost<input$range,]$lat)
      
      print (df)
      #print(input$checkGroup)
      if(nrow(df)==0){
        print ('here')
        m <- leaflet(geoloc) %>%
          addProviderTiles("MtbMap") %>%
          addProviderTiles("Stamen.TonerLines",
                           options = providerTileOptions(opacity = 0.35)
          ) %>%
          addProviderTiles("Stamen.TonerLabels") %>%  # Add default OpenStreetMap map tiles
          addCircleMarkers(~lon, ~lat,
                           radius = 4,
                           stroke = FALSE, fillOpacity = 0.5)%>%
        setView(-73.97694,40.74554,zoom = 12)
       }else{
        m <- leaflet(df) %>%
          addProviderTiles("MtbMap") %>%
          addProviderTiles("Stamen.TonerLines",
                           options = providerTileOptions(opacity = 0.35)
          ) %>%
        addProviderTiles("Stamen.TonerLabels")%>%
         addCircleMarkers(~lon, ~lat,
         radius = 4,
         stroke = FALSE, fillOpacity = 0.5)
       }
      m
      })
  
  dygraph_data = reactive({
    
    validate(need(!is.null(input$District), "Select District first"))
    subset = filter(final, area %in% input$District) %>%
      group_by(area,SALE.DATE) %>% 
      summarise(avg = mean(cost.FT2,na.rm=T),count=n()) %>%
      select(SALE.DATE, avg, area)
    subset <- spread(subset, area, avg)
    subset <- data.frame(subset[, input$District, drop = FALSE], row.names = subset$SALE.DATE)
    subset
  })

  
   output$dygraph <- renderDygraph({

     dygraph(dygraph_data())

    })
   
   output$tbl = renderDataTable(final
   )
   
   output$neighbourhood = renderUI({
     
     neighbourhoods <- unique(filter(final, area %in% input$area) %>% select(NEIGHBORHOOD))
     selectInput('neighbour',label=h3('Neighborhood'), choices = neighbourhoods)
   })
})
  
