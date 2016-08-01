library(shiny)
library(DT)
library(threejs)
library(rgdal)
library(maptools)
library(dplyr)
library(googleVis)
library(NISTunits)

shinyServer(function(input, output, session) {
  
  ##=====================================================================>
  ## TAB: MAP
  ##=====================================================================>
  #only show map theme option on map page
  output$theme <- renderUI({
    if (input$menu == "map") {
      return(
          div(
              HTML('<hr style="color: purple;">'),
              HTML('<h4 style="padding-left: 12px; padding-bottom: 0px; margin: 0px;">Appearance</h4>'),
              selectizeInput("theme", "Map Theme:", themes)
          )
        ) 
    } 
  })
  #set theme
  observeEvent(input$theme, {
    earth <- system.file("images/world.jpg",  package="threejs")
    if (input$theme == 'Terrain') {
      earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg"
    }
    values$theme <- earth
  })
  #dont show timer on info page
  # output$slider <- renderUI({
  #   if (input$menu != "info") {
  #       return(
  #         sliderInput("slider", "Time", 
  #           min(ihs$Date, na.rm=TRUE), 
  #           max(ihs$Date, na.rm=TRUE), 
  #           value= c(min(ihs$Date, na.rm=TRUE), min(ihs$Date, na.rm=TRUE) + 30), 
  #           step = 7,
  #           timeFormat='%v',
  #           animate=animationOptions(interval=250, loop=T))
  #       )
  #   }
  # })
  output$daterange <- renderUI({
    dateRangeInput("daterange", "Date range:",
                 start = input$slider[1],
                 end  = input$slider[2],
                 min= min(ihs$Date, na.rm=TRUE),
                 max = max(ihs$Date, na.rm=TRUE),
                 format = "dd-mm-yyyy")
  })

  values <- reactiveValues(theme = system.file("images/world.jpg",  package="threejs"),
                           lat = 0, long = 0, data = NULL, nodest= NULL)
 
  #main slider filter
  observeEvent(input$slider, {
    #no destinations
    #filter out NA destinations
    data <- ihs[!is.na(ihs$Destination.Country.Lat) & !is.na(ihs$Source.Country.Lat),]
    #filter out all routes where destination is same as source
    data <- data[data$Source..1. != data$Primary.Destination.Country,] 
    #filter data based on date slider 
    values$data <- data[data$Date >= input$slider[1] & data$Date <= input$slider[2],] 
    #TODO if animation button is on only  
    values$lat <- values$lat + pi/180 * .25 #rotate up 
    values$long <- values$long + pi/180 * -1 #rotate right
  })
  # 
  degrees.to.radians<-function(degrees,minutes) {
      decimal<-minutes/60
      c.num<-degrees+decimal
      radians<-c.num*pi/180
      radians
  }
  #main filter function 
  filterData <- reactive({
    if(!is.null(input$filter)) {
      #animate to last selected country ##TODO PROPER CONVERSION FORMULA
      values$lat <- (countries[countries$Country == input$filter[length(input$filter)], ]$Latitude*pi)/180
      values$long <- (countries[countries$Country == input$filter[length(input$filter)], ]$Longitude*pi)/180
      # lat <- strsplit(as.character(countries[countries$Country == input$filter[length(input$filter)], ]$Latitude), '\\.')
      # long <- strsplit(as.character(countries[countries$Country == input$filter[length(input$filter)], ]$Longitude), '\\.')
      # values$lat <-  degrees.to.radians(as.numeric(lat[[1]][1]),as.numeric(lat[[1]][2]))
      # values$long <- degrees.to.radians(as.numeric(long[[1]][1]),as.numeric(long[[1]][2]))
      return(values$data[values$data[,input$radio] %in% input$filter,])
    } else {
      return(values$data)
    }
  })
  output$filter <- renderUI({
      if (input$radio == 'Source..1.') {
        label <- 'Source:'
      } else {
        label <- 'Destination:'
      }
      columns <-  unique(values$data[ ,input$radio])
      # print(sort(columns))
      selectInput("filter", paste(label, sep=''),  
          c("All" = "", columns), 
          selectize=TRUE, 
          multiple=TRUE) 
  })
  
  output$value <- renderValueBox({
      if (is.null(input$filter)) {
          #get opposite label and input than what is selectedon radio button
          label  <- ifelse(input$radio == 'Source..1.', 'Export', 'Import')
          radio <- ifelse(input$radio == 'Source..1.', 'Source..1.', 'Primary.Destination.Country')
          #use all data if not filtered
          if (is.null(input$filter)) {
            data <- values$data
          } else {
            data <- filterData()
          }
          #summarize filtered data on radio button select - sort desc on sum
          data <- arrange(data %>% group_by_(radio) %>% summarise(sum=sum(volume)), desc(sum))
          valueBox(
            data[1,1], 
            paste("Greatest ", label,' Country \n ',prettyNum(data[1,2], scientific=FALSE, big.mark=','), 'MT', sep=''),
            color = "green"
          )
      } else {
          #get opposite label and input than what is selectedon radio button
          label  <- ifelse(input$radio == 'Source..1.', 'Export', 'Import')
          radio <- ifelse(input$radio == 'Source..1.', 'Source..1.', 'Primary.Destination.Country')
          #use all data if not filtered
          if (is.null(input$filter)) {
              data <- values$data
          } else {
              data <- filterData()
          }
          #summarize filtered data on radio button select - sort desc on sum
          data <- arrange(data %>% group_by_(radio) %>% summarise(sum=sum(volume)), desc(sum))
          valueBox(
            paste(input$filter, collapse = ', '), 
            paste("Greatest ", label,' Country \n ', prettyNum(data[1,2], scientific=FALSE, big.mark=','), 'MT', sep=''),
            color = "green"
          )
      }
  })
  
  output$value1 <- renderValueBox({
    #set label
    if (is.null(input$filter)) {
      label <- 'Volume'
    } else if (input$radio == 'Source..1.') {
      label <- 'Exports'
    } else {
      label <- 'Imports'
    }
    #use all data if not filtered
    if (is.null(input$filter)) {
      data <- values$data
    } else {
      data <- filterData()
    }
    valueBox(
      paste(prettyNum(sum(data$volume, na.rm=TRUE), scientific=FALSE, big.mark=','), " MT"), paste('Total ', label, ' (C3+C4)', sep=''),
      color = "purple"
    )
  })
  
  output$value2 <- renderValueBox({
      #get opposite label and input than what is selectedon radio button
      label  <- ifelse(input$radio == 'Source..1.', 'Import', 'Export')
      radioOpposite <- ifelse(input$radio == 'Source..1.', 'Primary.Destination.Country', 'Source..1.')
      #use all data if not filtered
      if (is.null(input$filter)) {
          data <- values$data
      } else {
          data <- filterData()
      }
      #summarize filtered data on opposite of radio button select - sort desc on sum
      data <- arrange(data %>% group_by_(radioOpposite) %>% summarise(sum=sum(volume)), desc(sum))
      valueBox(
          data[1,1], 
          paste("Greatest ", label,' Country \n ',prettyNum(data[1,2], scientific=FALSE, big.mark=','), 'MT', sep=''),
          color = "orange"
      )
  })
  
  # data(wrld_simpl)                             # Basic country shapes
  # load(url("http://illposed.net/bono.rdata"))  # Awareness index data
  # 
  # bgcolor <- "#000025"
  # earth <- tempfile(fileext=".jpg")
  # jpeg(system.file("images/world.jpg",  package="threejs"), width=2048, height=1024, quality=100, bg=bgcolor, antialias="default")
  # par(mar = c(0,0,0,0),    pin = c(4,2),    pty = "m",    xaxs = "i",
  #     xaxt = "n",          xpd = FALSE,    yaxs = "i",    yaxt = "n")
  # 
  # map_palette <- apply(col2rgb(heat.colors(5)[5:1])/768,2,function(x)rgb(x[1],x[2],x[3]))
  # # Restrict bono data to countries from the maptools package
  # bono <- bono[rownames(bono) %in% wrld_simpl$NAME, ,drop=FALSE]
  # # Set a default color for each country and the colors from the bono data
  # clrs <- rep(map_palette[1], length(wrld_simpl$NAME))
  # names(clrs) <- wrld_simpl$NAME
  # clrs[rownames(bono)] <- map_palette[bono$index]
  # 
  # plot(wrld_simpl,  col="#FFFFF",   bg=bgcolor,        border="cyan",  ann=FALSE,
  #      axes=FALSE,  xpd=FALSE,  xlim=c(-180,180), ylim=c(-90,90),  setParUsrBB=TRUE)
  # # 
  # graphics.off()
  # legendcol=heat.colors(5)[5:1]
  ##arcs
  # portFlow   <- select(data,Source.Port.Lat,Source.Port.Long,Destination.Port.Lat,Destination.Port.Long)
  
  #set data to be render in arcs
  flow <- reactive({
    return(select(filterData(),Source.Country.Lat,Source.Country.Long,Destination.Country.Lat,Destination.Country.Long))
  })
  #arc thickness based on volume
  volume <- reactive({
    return((filterData()$volume / max(filterData()$volume, na.rm=TRUE)) * 4)
  })
  # arcsHeight <- round(((volume / max(volume, na.rm=TRUE)) * .49) + .5, 2)
  # set globe
  output$globe <- renderGlobe(
      globejs(
          img=values$theme,
          # lat=data()$Source.Country.Lat,
          # long=data()$Source.Country.Lat,
          # val=,    # Bar height
          # color=data()$color,
          arcs=flow(),
          arcsHeight=.9, 
          arcsLwd=volume(), 
          arcsColor=filterData()$color, 
          arcsOpacity=0.25,
          pointsize=0.5,
          rotationlat=values$lat,
          rotationlong=values$long,
          fov=50,
          atmosphere=TRUE
      )
  )
  
  ##=====================================================================>
  ## TAB: DATA
  ##=====================================================================>
  output$table <- DT::renderDataTable({
    datatable(
      transmute(filterData(), 
                'Date'=Date,
                'Source Country'=Source..1.,
                'Destination Country'=Primary.Destination.Country, 
                'Vessel Name'=Vessel.Name,
                'Vessel Size (CBM)'=Vessel.Size..CBM.,
                'Volume C3+C4 (MT)'=volume),
      rownames=values$data$Participant..1.          
      # filter = 'top'
    )
    # %>% formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
  
  ##=====================================================================>
  ## TAB: FLOW
  ##=====================================================================>
  output$sankey <- renderGvis({
      #use all data if not filtered
      if (is.null(input$filter)) {
        flow <- values$data
      } else {
        flow <- filterData()
      }
      #groupby source country and destination so as to combine volumes of flow to the same destination. 
      flow <- flow %>% group_by(Source..1., Primary.Destination.Country) %>% summarise(volume=sum(volume)) %>% arrange(desc(volume))
    
      #limit to 25 most volumeous trades
      flow <- flow[1:25,]
      flow <- data.frame(From=flow$Source..1.,
                       To=flow$Primary.Destination.Country,
                       Volume=flow$volume)
      result <- gvisSankey(flow, from="From", to="To", weight="Weight", 
         options=list(
             width='100%',
             height=600,
             sankey="{link: {colors: ['#0055ff','#00aaff','#00ffaa','#aaff00'],colorMode: 'gradient' },
             node: { colors: ['#0055ff','#00aaff','#00ffaa','#aaff00']},
             label: { color: 'purple' } }")
         )
      
      result
  })
  
  ##=====================================================================>
  ## TAB: INFO
  ##=====================================================================>
})
