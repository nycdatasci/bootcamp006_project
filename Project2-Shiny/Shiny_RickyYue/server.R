library(shiny)
library(rgdal)
library(leaflet)
library(dplyr)
library(ggvis)

server = function(input, output) { 
  
  season_select <- reactive({
    season_select <- input$sea
    
  })
  
  data_select_season <- reactive({
    df_lea = df_sea %>% filter(season==season_select())
    ## join the map table with entropy table to create a shp file for leaflet
    oo <- inner_join(league_countries@data, df_lea, by=c("ID"="country"))
    data_select_season=league_countries 
    data_select_season@data=oo
    data_select_season
  })
  
  
  league_select <- reactive({
    league_select <- input$lea
    
  })
  
  data_select_league <- reactive({
    data_select_league=df_sea%>% filter(country==league_select())
    data_select_league
  })
  
  
  output$myMap = renderLeaflet({
    
    ## league avatar icon address
    eng <- "C:/Users/ricky/Dropbox/bootcamp/eng.png"
    sco <- "C:/Users/ricky/Dropbox/bootcamp/sco.png"
    ita <- "C:/Users/ricky/Dropbox/bootcamp/ita.png"
    spa <- "C:/Users/ricky/Dropbox/bootcamp/spa.png"
    ger <- "C:/Users/ricky/Dropbox/bootcamp/ger.png"
    ned <- "C:/Users/ricky/Dropbox/bootcamp/ned.png"
    fra <- "C:/Users/ricky/Dropbox/bootcamp/fra.png"
    por <- "C:/Users/ricky/Dropbox/bootcamp/por.png"
    
    mypal <- colorNumeric("Blues",data_select_season()@data$entropy)
    leaflet(data_select_season()) %>% 
      addTiles()%>%
      addLegend("bottomright", pal = mypal, values = data_select_season()@data$entropy, title = "Preditibility", opacity = 0.5)%>%  
      addMarkers(0.1278,51.5074, popup = paste0("<img src = ", eng, ">")) %>%
      addMarkers(2.3522,48.8566, popup = paste0("<img src = ", fra, ">")) %>%
      addMarkers(13.4050,52.5200, popup = paste0("<img src = ", ger, ">")) %>%
      addMarkers(-9.1393,38.7223, popup = paste0("<img src = ", por, ">")) %>%
      addMarkers(-3.7038,40.4168, popup = paste0("<img src = ", spa, ">")) %>%
      addMarkers(-3.1883,55.9533, popup = paste0("<img src = ", sco, ">")) %>%
      addMarkers(12.4964,41.9028, popup = paste0("<img src = ", ita, ">")) %>%
      addMarkers(4.8952,52.3702, popup = paste0("<img src = ", ned, ">"))%>%
      addPolygons(smoothFactor = 0.2, fillOpacity = 1,color = ~mypal(entropy), stroke = FALSE)
  })
  
  output$myLinechart=renderPlot({
    ggplot(data=data_select_league(), aes(x=season, y=entropy)) +
      ggtitle(league_select())+
      geom_line() + 
      labs(x="Season",y="Predictibility")+
      geom_point(size=6, shape=20, fill="blue") + 
      theme(plot.title = element_text(size=24,face="bold",color="deepskyblue2"))+
      theme(axis.title = element_text(size=24,face="bold",color="deepskyblue2"))
  })
}
