require(dplyr)
library(ggplot2)
library(ggvis)
library(plotly)
library(RColorBrewer)
function(input, output, session){
  
  nyczipcodecount <- reactive({
    
    date_input <- as.numeric(which(numbers_date$date == as.character(input$slt_date)) -1)
    # isolate(input$hour <- unique(input$hour))
    
    if(length(date_input) == 0)
      date_input <- 0
    print(date_input)
    print(input$hour)
    count_tojoin = taxidata %>%
      filter(DayofWeek == as.character(date_input)) %>%
      filter(Hour == 0) %>%
      group_by(Zipcode) %>%
      summarise(Total = sum(Count)) %>%
      mutate(Zipcode=factor(Zipcode))
    # print(head(count_tojoin))
    
    # nyczipcode@data = merge(x = nyczipcode@data, y = count_tojoin,
    #                         by.x='GEOID10', by.y='Zipcode', all.x = TRUE)
    
    total <- count_tojoin$Total
    names(total) <- count_tojoin$Zipcode
    nyczipcodecount <- nyczipcode
    nyczipcodecount@data$Total <- total[as.character(nyczipcode@data$GEOID10)]
    nyczipcodecount
    
  })
  ## Street and Choropleth Map
  # Create and join instant time variable to map shapefile
  # colour palette mapped to data
  
  # Join predicted count to map shapfile
  pal <- colorQuantile("YlGn", nyczipcode@data$Total , n = 10) 
  
  output$map <- renderLeaflet({
    basemap <- leaflet() %>%
      # leaflet::addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(data=nyczipcodecount(),
                           fillColor = pal(nyczipcodecount()@data$Total),
                           fillOpacity = 0.8,
                           color = "#BDBDC3",
                           stroke = TRUE,
                           weight = 2,
                           layerId = nyczipcodecount()@data$GEOID10) %>%
                           # popup = count_popup) %>%
      # addMarkers(lng = -73.97,lat = 40.74) %>%
      setView(lng = -73.97,lat = 40.74, zoom = 13)
    basemap
    })
  
  # Record markers on plot
  click_event <- reactiveValues()
  
  observeEvent(input$map_shape_click, {
    event <- input$map_shape_click

    if (is.null(event))
      return()

    leafletProxy("map") %>%
      addMarkers(lng=event$lng, lat=event$lat)

    isolate(click_event$Zipcode <- unique(c(click_event$Zipcode, event$id)))
    # print(click_event$Zipcode)

  })

  observe(print(input$slt_date))
  observe(print(typeof(as.character(input$slt_date))))

 # Record marker stats
 reactive({

   pt <- click_event$Zipcode
   date_input <- as.numeric(which(numbers_date$date == as.character(input$slt_date)) -1)
   sl <- input$slt_ptype
   hr <- input$hour

   if(is.null(taxidata)|is.null(pt)|is.null(input$slt_date)){
     p <- data.frame(x=1,y=1) %>%
       ggvis(~x,~y) %>%
       layer_points()
   } else {

     if(sl=='Model Compare'){
       p = taxidata %>%
         filter(DayofWeek %in% date_input) %>%
         filter(Zipcode %in% pt) %>%
         filter(Hour %in% input$hour) %>%
         group_by(Zipcode,Hour,Type) %>%
         summarise(sum(Count)) %>%
         mutate(Zipcode_new = factor(Zipcode)) %>%
         mutate(Type_Zipcode = factor(paste(Type, Zipcode_new))) %>%
         ggvis(x=~Type_Zipcode, y=~`sum(Count)`, fill=~Zipcode_new) %>%
         layer_bars(stack = FALSE)
       p
     }

     else{  # (sl=='Timeseries')
       p = taxidata %>%
         filter(DayofWeek %in% date_input) %>%
         filter(Zipcode %in% pt) %>%
         group_by(Zipcode, Hour) %>%
         summarise(sum(Count)) %>%
         mutate(Zipcode_new = factor(Zipcode)) %>%
         ggvis(~Hour, ~`sum(Count)`, stroke=~Zipcode_new, strokeWidth := 3) %>%
         layer_lines() %>% scale_ordinal("stroke",
         range = brewer.pal(7, "Set1"))
       p
     }


     }
   }) %>% bind_shiny(plot_id = "P")

 
 observeEvent(input$btn_clr,{
   
   leafletProxy('map') %>%
     clearMarkers()
   
 })

  output$plot_type <- renderUI({

    selectInput('slt_ptype',label = NULL,
                choices = c('Model Compare','Timeseries'),selectize = F)

  })

  output$hour_output <- renderUI({
    
    sliderInput("hour", "Hour",
                min=0, max=23, value=0)
    
  })
  
  output$plot_UI <- renderUI({
    absolutePanel(id = "controls",
                  bottom = 60,
                  right = 10,
                  draggable = T,
                  width='auto',
                  height='auto',
                  ggvisOutput(plot_id = "P"))
  })


  ## Data Table
  output$table <- renderDataTable({
    taxidata})


}





