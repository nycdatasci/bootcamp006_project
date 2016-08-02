library('ggmap')
library('mapproj')
library('leaflet')
library('shiny')
library('ggplot2')
# source('global.R')
library(dygraphs)
library(dplyr)
library(tidyr)
library(htmltools)

#outputs  
shinyServer(function(input,output){
  df = reactive({
    if (input$neighbour=="All"){
      
      df = data.frame(lon = final[
         final$BEDS %in% input$select &
                                    final$BATHS %in% input$select2 &
                                    final$area %in% input$area &
                                    final$cost<input$range,]$lon,
                      lat = final[
                         final$BEDS %in% input$select & 
                                     final$BATHS %in% input$select2 &
                                     final$area %in% input$area &
                                    final$cost<input$range,]$lat)

      }else{
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
      }
    
    
  })
  output$map <- renderLeaflet({
      # 
      #print(input$checkGroup)
      # if(nrow(df)==0){
      #    m <- leaflet(geoloc) %>%
      #     addProviderTiles("MtbMap") %>%
      #     addProviderTiles("Stamen.TonerLabels",
      #                      options = providerTileOptions(opacity = 0.35)
      #     ) %>%
      #     addProviderTiles("Stamen.TonerLabels") %>%  # Add default OpenStreetMap map tiles
      #     addCircleMarkers(~lon, ~lat,
      #                      radius = 4,
      #                      stroke = FALSE, fillOpacity = 0.5)%>%
      #   setView(-73.97694,40.74554,zoom = 12)
      #  }else{
      content <- paste(sep = "<br/>",
                       "<b><a href='https://www.cityrealty.com/nyc/market-insight/market-data/recent-sales#?page=1'>City Realty</a></b>",
                       as.character(final$ADDRESS)
      )
      
        if(nrow(df()) != 0){
                    m <- leaflet(df()) %>%
                      addProviderTiles("Stamen.Toner",options = providerTileOptions(opacity = 0.35))%>%
                      addMarkers(clusterOptions = markerClusterOptions(),popup=content)#,icon = Icon)
        #addPopups(content,options = popupOptions(closeButton = FALSE))
        }else{
          if(input$area == ' '){
            
          m <- leaflet(geoloc) %>%
            addProviderTiles("Stamen.Toner",options = providerTileOptions(opacity = 0.35))%>%  # Add default OpenStreetMap map tiles
            addCircleMarkers(~lon, ~lat,
                             radius = 4,
                             stroke = FALSE, fillOpacity = 0.4,popup = content)%>%
            #addPopups(content,options = popupOptions(closeButton = FALSE)) %>%
              setView(-73.97694,40.74554,zoom = 12)
         }else{
            m <- leaflet(df()) %>%
              addProviderTiles("Stamen.Toner",options = providerTileOptions(opacity = 0.35)) %>%  # Add default OpenStreetMap map tiles
              setView(-73.97694,40.74554,zoom = 12)
          }
        }
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
   
   output$tbl = renderDataTable(mydata)
   
   output$neighbourhood = renderUI({
     
     neighbourhoods <- unique(filter(final, area %in% input$area) %>% select(NEIGHBORHOOD))
     selectInput('neighbour',label=h3('Neighborhood'), choices = c("All",neighbourhoods), selected = "All",multiple = F)
   })
})
  
