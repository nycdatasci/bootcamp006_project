


shinyServer(function(input,output){
  
  
  selected_df= reactive({
    
    df = yelpBusiness1
    if(input$RestaurantType != "All") { 
      df = df %>%
        filter(NewCategory == input$RestaurantType)
    }
    
    if(input$PriceRange != "All") {
      df = df %>%
        filter(PriceCategory == input$PriceRange)
    }
    
    if(input$RatingType != "All") {
      df = df %>%
        filter(stars_of_business == input$RatingType)
    }
    return (df)
    
  })  
  
  
  ################################### Leaflet ######################################################333333
  
  

  output$map =renderLeaflet({
    palColor = colorFactor(c("blue", "red"),
                           domain = c("Above Average", "Below Average"))
    
    YelpIcons <- icons(
      iconUrl = ifelse(selected_df()$NewRating == "Above Average",
                       "https://camo.githubusercontent.com/afa9cd3e3fde5e3768f0061f4a1d330d0cb25383/68747470733a2f2f7261772e6769746875622e636f6d2f706f696e7468692f6c6561666c65742d636f6c6f722d6d61726b6572732f6d61737465722f696d672f6d61726b65722d69636f6e2d626c75652e706e673f7261773d74727565",
                       "https://camo.githubusercontent.com/70c53b19fb9ec32c09ff59b4aebe6bb8058dfb8b/68747470733a2f2f7261772e6769746875622e636f6d2f706f696e7468692f6c6561666c65742d636f6c6f722d6d61726b6572732f6d61737465722f696d672f6d61726b65722d69636f6e2d7265642e706e673f7261773d74727565"
      ),
      iconWidth = 10, iconHeight = 10,
      iconAnchorX = 10, iconAnchorY = 10
    )

    leaflet(selected_df()) %>%
      addTiles() %>%  
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      addMarkers(icon = YelpIcons,
                 clusterOptions = markerClusterOptions(disableClusteringAtZoom = 15,
                                                       # animateAddingMarkers= TRUE,
                                                       animate = TRUE),
                popup = ~ paste( sep = "<br/>",
                                 "<b>Description</b>",
                                 paste('<b>business_id : </b>', business_id,sep = ' '  ),
                                 paste('<b>stars :</b>', stars_of_business, sep = ' '),
                                 paste('<b>category:</b>',  categories, sep = ' '),
                                 paste('<b>Avg Category star:</b>',  AVG_stars_category, sep = ' ')
                )) %>%
      setView(lng = -112,
              lat = 33.5,
              zoom = 12)   %>%
      addLegend("topleft", pal = palColor,
                values = ~ selected_df()$NewRating,
                title = "Type of Restaurants",
                opacity = 0.5   )
  })
  
  
 
 #######################################3 # plot inside Leaflet #######################################################3
  
  datatable22 = reactive({
    datatable2 = as.data.table( selected_df(), rownames=FALSE)

    if (is.null(input$map_bounds))
      return(datatable2[FALSE,])
    bounds = input$map_bounds
    latRng = range(bounds$north, bounds$south)
    lngRng = range(bounds$east, bounds$west)

    datatable2[latitude >= latRng[1] & latitude <= latRng[2] &
                 longitude >= lngRng[1] & longitude <= lngRng[2]]

  })


  output$barplot1 = renderPlot({

    df2 = as.data.frame(datatable22())
    df_plot2 = df2  %>%
            group_by(NewRating) %>%
            summarise(Total = n())


    color1 = c("blue","red")
    my_func = function(x){
      if(x=='Above Average') return ('1')
      else  return ('2')
    }

    names(color1) = c('1','2')
    color1 = color1[sapply(unique(df_plot2$NewRating), my_func)]
    
    g = ggplot(data=df_plot2,
                aes(x=factor(NewRating),y=Total))
    g = g + geom_bar(stat='identity', fill=color1)
    g = g + xlab('Type of Rating')
    g = g + ylab('Frequency')
    g = g + theme_minimal()
    #g = g + ylim(0,2000)
    g
  })
  
  
  
  
  #-#####################################3Datatable ############################################################3
  
  # show data using DataTable
  output$table = DT::renderDataTable({
    
    datatable( 
      yelpBusiness1,  rownames=FALSE) %>%
              formatStyle(input$selected,
                           background="skyblue", fontWeight='bold')
    
  })
  
  
  
  
  
  #######################################3 LDA Selectbox ###############################################################3
  
  LDAType= reactive({
    return (input$LDAid)
  })
  
  output$myChartLDASelect <- renderVis({
    if (LDAType() == "1"){
      json_American_Positive
    }
    else if (LDAType() == "2"){
      json_Chinese_Positive
    }
    else if (LDAType() == "3"){
      json_Italian_Positive
    }
    else if (LDAType() == "4"){
      json_Thai_Positive
    }
    else{
      json_Mexican_Positive
    }
    
  })
  
  
  ################################################### Barplots ###############################################3
  
  output$plothour = renderGvis({
    gvisColumnChart(data = checkingtime_time,
                    xvar = "hours",
                    yvar = "checkingTotal",
                    options=list(
                      chartArea=  "{left:110,top:50,width:'81%',height:'55%'}",
                      seriesType="bars",
                      legend ="top",
                      title="Total number of check-in by time of the day ",
                      height = 600,
                      width = 1200,
                      hAxis= "{title: 'Hours'}",
                      vAxis= "{title: 'Total Count of Check-in '}"
                    ))
    
  })
  
  output$plotdays = renderGvis({
    gvisColumnChart(data = checkingtime_days,
                    xvar = "days",
                    yvar = "checkingTotal",
                    options=list(
                      chartArea=  "{left:110,top:50,width:'75%',height:'55%'}",
                      seriesType="bars",
                      legend ="top",
                      bar="{groupWidth:'40%'}",
                      title="Total number of check-in by days of the week ",
                      height = 600,
                      width = 1180,
                      hAxis= "{title: 'Days of the week'}",
                      vAxis= "{title: 'Total Count of Check-in '}"
                    ))
    
  })
  
  
  #dayhours
  checkingtime_dayhours = checkingTime %>%
    arrange(desc(check_in_number)) %>%
    top_n(15)
  checkingtime_dayhours['dayHour'] = paste(checkingtime_dayhours$days, checkingtime_dayhours$hours)
  checkingtime_dayhours$dayHour <- factor(checkingtime_dayhours$dayHour, 
                                          levels=unique(checkingtime_dayhours$dayHour))
  
  
  checkingtime_dayhours %>% 
    ggvis(~dayHour, ~check_in_number, fill=~days) %>%
    layer_bars(width = 0.5) %>%
    set_options(height = 600, width = 1100) %>%
  bind_shiny("plotdaytime")
  
  
  
  ############################## Image Selection  #########################################################3333333
  
  ImageType= reactive({
    return (input$Imageid)
  })
  
  output$ImageSelect <- renderUI({
    
    
    if (ImageType() == "1"){
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           <img src="img/American/baked-mac-and-cheese.jpg" >
           <img src="img/American/pulledpork.jpg" >
           <img src="img/American/frenchtoast.jpg" >
           <img src="img/American/sweet_potato_fries.jpg" >
           <img src="img/American/primerib.jpg" >
           <img src="img/American/beer.jpg" >
           <img src="img/American/mashedpotatoes.jpg" >
           <img src="img/American/shortribs.jpg" >
           <img src="img/American/eggsbenedict.jpg" >
           <img src="img/American/porkbelly.jpg" >
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    else if (ImageType() == "2"){
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <img src="img/Chinese/friedrice.jpg" >
           <img src="img/Chinese/orangechicken.jpg" >
           <img src="img/Chinese/sweetsourpork.jpg" >
           <img src="img/Chinese/lomein.jpg" >
           <img src="img/Chinese/crabpuffs.jpg" >
           <img src="img/Chinese/eggdropsoup.jpg" >
           <img src="img/Chinese/hotsoursoup.jpg" >
           <img src="img/Chinese/eggroll.jpg" >
           <img src="img/Chinese/dimsum.jpg" >
           <img src="img/Chinese/TeriyakiChicken.jpg" >
           
           
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    else if (ImageType() == "3"){
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <img src="img/Italian/thincrustpizza.jpg" >
           <img src="img/Italian/garlicknots.jpg" >
           <img src="img/Italian/marinarasauce.jpg" >
           <img src="img/Italian/chickenparm.jpg" >
           <img src="img/Italian/oliveoil.jpg" >
           <img src="img/Italian/chickenmarsala.jpg" >
           <img src="img/Italian/margheritapizza.jpg" >
           <img src="img/Italian/goatcheese.jpg" >
           <img src="img/Italian/capresesalad.jpg" >
           <img src="img/Italian/woodfiredpizza.jpg" >
           
           
           
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    else if (ImageType() == "4"){
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <img src="img/Thai/padthai.jpg" >
           <img src="img/Thai/FriedRice.jpg" >
           <img src="img/Thai/RedCurry.jpg" >
           <img src="img/Thai/GreenCurry.jpg" >
           <img src="img/Thai/thaibasil.jpg" >
           <img src="img/Thai/panangcurry.jpg" >
           <img src="img/Thai/ThaiStickyRice.jpg" >
           <img src="img/Thai/Drunkennoodles.jpg" >
           <img src="img/Thai/springrolls.jpg" >
           <img src="img/Thai/thaiicedtea.jpg" >
           
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    
    
    
    else{
      
      HTML('
           <link rel="stylesheet" href="css/example.css">
           <link rel="stylesheet" href="css/font-awesome.min.css">                         
           <link rel="stylesheet" href="css/style.css">
           <script src="js/jquery.slides.min.js"></script>  
           
           <script>
           $(function() {
           $("#slides").slidesjs({
           width: 940,
           height: 528,
           navigation: false
           });
           });
           </script>
           
           <div class="container">
           <div id="slides">
           
           <img src="img/Mexican/carneasadaburrito.jpg" >
           <img src="img/Mexican/salsachips.jpg" >
           <img src="img/Mexican/fishtacos.jpg" >
           <img src="img/Mexican/streettacos.jpg" >
           <img src="img/Mexican/salsabar.jpg" >
           <img src="img/Mexican/alpastor.jpg" >
           <img src="img/Mexican/greenchilis.jpg" >
           <img src="img/Mexican/flourtortillas.jpg" >
           <img src="img/Mexican/redsalsa.jpg" >
           <img src="img/Mexican/refriedbeans.jpg" >
           
           
           
           
           <a href="#" class="slidesjs-previous slidesjs-navigation"><i class="icon-chevron-left icon-large"></i></a>
           <a href="#" class="slidesjs-next slidesjs-navigation"><i class="icon-chevron-right icon-large"></i></a>
           </div>
           </div>
           ')
    }
    
    
    })
  
   
})