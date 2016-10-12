
server <-

  function(input, output, session) {


    
    data_all <- reactive({
      data_all <- filter(api_all, vintage == input$yearSlider)
      data_all <- filter(data_all, wine_type_name == input$wineType)
      data_all <- filter(data_all, varietal == input$varietalName)
      if(input$appellationSelect != "All"){
        data_all <- filter(data_all, appellation_name == input$appellationSelect)
      }
      
      return(data_all)
      
    })
    
    data_all2 <- reactive({
      data_all2 <- filter(api_all, vintage == input$yearSlider)
      data_all2 <- filter(data_all2, wine_type_name == input$wineType)
      data_all2 <- filter(data_all2, varietal == input$varietalName)
      data_all2 <- filter(data_all2, avg_price < input$priceSlider)
      if(input$appellationSelect != "All"){
        data_all2 <- filter(data_all2, appellation_name == input$appellationSelect)
      }
      
      return(data_all2)
      
    })
    
    
    # 
    # 
    pdata <- reactive({
      rdata <- c_agro_all
      rdata <- filter(rdata, api_wine_type_name == input$wineType)
      
      #agro data has many to many mapping, so replacing some values accordingly
      rdata <- filter(rdata, api_appellation ==
                        ifelse(
                          input$appellationSelect == "Carneros","Napa Valley", ifelse(
                            input$appellationSelect == "Santa Maria Valley", "Central Coast", ifelse(
                              input$appellationSelect %in% c("North Coast", "Russian River"), "Sonoma County", input$appellationSelect
                            ))))
      return(rdata)
    })
    # 
    # 
    # 
    output$varietal_name <- renderUI({
      
      
      my_choice = ifelse(input$appellationSelect != "All",
                         ungroup(varietal_df %>% arrange(wine_type_name, varietal) %>% filter(wine_type_name == input$wineType & appellation_name == input$appellationSelect))[,3] %>%unique(),
                         ungroup(varietal_df %>% arrange(wine_type_name, varietal) %>% filter(wine_type_name == input$wineType ))[,3] %>%unique())
      my_choice = my_choice[[1]]
      names(my_choice) = my_choice

      selectInput(


        "varietalName", label = "Select Varietal(s):", choices = my_choice,
        multiple = TRUE, selected = "All Grapes" )

      
    })
    
    output$PriceBox <- renderValueBox({
      dta <- data_all()
      
      valueBox(
        sprintf("$ %3.2f", mean(dta$avg_price, na.rm = TRUE)),
        "Average Price", icon = icon("money"), color = "light-blue")
    })
    
    output$ScoreBox <- renderValueBox({
      dta <- data_all()
      
      valueBox(
        ifelse(is.na(mean(dta$avg_score, na.rm = TRUE)),"No Average Score",sprintf("%3.1f", mean(dta$avg_score, na.rm = TRUE))),
        "Average Score (out of 100)", icon = icon("yelp"), color = "light-blue")
    })
    
    output$ValueBox <- renderValueBox({
      dta <- data_all()
      
      valueBox(
        ifelse(is.na(mean(dta$avg_value_score, na.rm = TRUE)),"No Average Score",sprintf("%3.1f", mean(dta$avg_value_score, na.rm = TRUE))),
        "Value Rating (Score to Price Ratio for Wines Scored 93+)", icon = icon("balance-scale"), color = "light-blue")
    })
    
    output$TotalBox <- renderValueBox({
      dta <- data_all()
      
      valueBox(
        sprintf("%i", sum(dta$wine_count)),
        "Total Wines in Catalog", icon = icon("glass"), color = "light-blue")
    })
    # 
    output$wineMap <- renderLeaflet({
      data <- data_all()
      leaflet(data)%>%
        addTiles() %>%
        # addPopups(~geo_long, ~geo_lat, ~tooltip, options = popupOptions(closeButton = TRUE)) %>%
        addCircleMarkers(lng=~geo_long, lat=~geo_lat, stroke = FALSE,
                         fillOpacity = switch(ifelse(input$scaleType == "Value Score", 1,ifelse(input$scaleType == "Price",2,3)),~score_scale,~price_scale,~volume_scale), 
                         color = "maroon", popup = ~tooltip)
    })
    
    
    output$apiScatter <- renderPlot({
      scat_data <- data_all2()
      ggplot(scat_data, aes(x=avg_score, y=avg_price)) + 
        theme_hc() +
        geom_point(size = 4, color = "blue") +
        theme(
          axis.text.x=element_text(size=14, vjust=0.5),
          axis.text.y=element_text(size=14, vjust=0.5),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          plot.title = element_text(size = 18, face = "bold")
          ) +
        scale_colour_hc(labels = "test") +
        labs(x = "Average Score", y = "Average Price per Bottle ($)") +
        scale_y_continuous(labels = scales::dollar, limits = c(0,input$priceSlider)) +
        ggtitle("Wine.com Catalog: Average Score vs Average Price per Bottle")
    }, width = 775, height = 500)
    
    
    
    # output$click_info <- renderPrint({
    #   scat_data <- data_all()
    #   nearPoints(scat_data, input$plot_click, xvar = "avg_score", yvar = "avg_price", maxpoints = 1)
    # })

    output$hover_info <- renderUI({
      
      scat_data <- data_all2()
      
      hover <- input$plot_hover
      
      point <- nearPoints(scat_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      
      if (nrow(point) == 0) return(NULL)
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      
      # from left (horizontal) and from top (vertical)
      
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      
      
      # calculate distance from left and bottom side of the picture in pixels
      
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      
      # create style property fot tooltip
      
      # background color is set so tooltip is a bit transparent
      
      # z-index is set so we are sure are tooltip will be on top
      
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      
      
      # actual tooltip created as wellPanel
      
      wellPanel(
        
        style = style,
        
        p(HTML(point$tooltip)))
    })
        

    
    
    output$line1 <- renderGvis({
      
      # tons chart
      tons <- pdata() %>% group_by(year, api_wine_type_name, api_appellation, api_varietal)  %>%
        summarise(val = mean(tons_crushed, na.rm=TRUE)/1000)
      tons <- tons%>%
        spread(api_varietal, val, fill = NA, convert = FALSE) %>%
        filter(api_appellation == input$appellationSelect)
      
      
      gvisColumnChart(tons, "year", input$varietalName[input$varietalName %in% names(tons)], options=
                     list(
                       title = "Annual Tons of Grapes Crushed in California, by Varietal",
                       fontSize = 16,
                       height = 350,
                       vAxes =
                         "[{title:'Tons Crushed',
                         format:'#,###K',
                         textPosition: 'out',}]",
                         legend =
                           "{position: 'bottom'}",
                         hAxes=
                           "[{format: '####',
                              gridlines: {count: '6'}}]"
                           )
                     )

    })
    

   
    output$line3 <- renderGvis({
      
      # price per ton chart
      price <- pdata() %>% group_by(year, api_wine_type_name, api_appellation, api_varietal)  %>%
        summarise(val = mean(avg_dollars_per_ton, na.rm=TRUE))
      price <- price %>%
        spread(api_varietal, val, fill = NA, convert = FALSE) %>%
        filter(api_appellation == input$appellationSelect)
      
      
      gvisLineChart(price, "year", input$varietalName[input$varietalName %in% names(price)], options=
                      list(
                        title = "Average Price per Ton ($) in California, by Varietal",
                        fontSize = 16,
                        height = 350,
                        lineWidth = 3,
                        # theme = 'maximized',
                        vAxes =
                          
                          "[{title:'$ per Ton',
                        format:'$#,###',
                        textPosition: 'out',}]",
                        legend =
                          "{position: 'bottom'}",
                        hAxes=
                          "[{format: '####'}]"
                      )
      )
    })
    
    output$line4 <- renderGvis({
      
      # avg brix
      brix <- pdata() %>% group_by(year, api_wine_type_name, api_appellation, api_varietal)  %>%
        summarise(val = mean(avg_brix_crushed, na.rm=TRUE))
      brix <- brix %>%
        spread(api_varietal, val, fill = NA, convert = FALSE) %>%
        filter(api_appellation == input$appellationSelect)
      
      
      gvisLineChart(brix, "year", input$varietalName[input$varietalName %in% names(brix)], options=
                      list(
                        title = "Average Brix Levels of Grapes Grown in California, by Varietal",
                        fontSize = 16,
                        height = 350,
                        lineWidth = 3,
                        # theme = 'maximized',
                        vAxes =
                          
                          "[{title:'Avg Degrees Brix',
                        format:'####.#',
                        textPosition: 'out',}]",
                        legend =
                          "{position: 'bottom'}",
                        hAxes=
                          "[{format: '####'}]"
                      )
      )
    })
     
  }
