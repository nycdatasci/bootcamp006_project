library(shinydashboard)
library(googleVis)
library(DT)
library(leaflet)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(rCharts)
library(rCharts)
library(data.table)


shinyServer(function(input,output){
  
  #------------------------------------------------reactive Functions------------------------------------------------
  NoOfBeds= reactive({
    return (as.numeric(input$Bedrooms))
  })  # search_tweet_df becomes a function
  
  NoOfBaths= reactive({
    return (as.numeric(input$Bathrooms))
  }) 
  NY_Neighborhood= reactive({
    return (input$Neighborhoods)
  })
  
  Rent_Min= reactive({
    validate(
      need(input$Rentmin != "", "Please Enter Minimum Price")
    )
    
    return (input$Rentmin)   
  })
  Rent_max= reactive({
    validate(
      need(input$Rentmax != "", "Please Enter Maximum Price")
    )
    return (input$Rentmax) 
  })
  
  
  # 
  # output$text1 = renderText({ 
  #   x = NY_Neighborhood()
  #   y = length(x)
  #   value = ''
  #   i = 1
  #   while (i <= y ){
  #     value = paste(value, paste("'",  NY_Neighborhood()[i], "' ,") );
  #     i = i +1 ;
  #   }
  #   value = substr(value, 1, nchar(value)-1)
  #   return (value)
  #   
  # })
  # 
  #-------------------------------------------------------------------------
  
  selected_df= reactive({
    
    df = IndividualApartment2
    if(input$Bedrooms != "All") { 
      df = df %>%
        filter(NoOfBedroom == as.numeric(input$Bedrooms))
    }
    
    if(input$Bathrooms != "All") { 
      df = df %>% 
        filter(NoOfBathRoom == as.numeric(input$Bathrooms))}
    
    
    df = df %>%
      filter(  
        # as.numeric(NoOfBathRoom) == NoOfBaths() ,
        Neighborhood %in% input$Neighborhoods ,
        RentRate  > input$Rentmin & RentRate < input$Rentmax
      )
    
    return (df)
    
  })  
  
  
  #-----------------------------------reactive functions-----------------------------------------------------
  
  
  
  #------------------------------------Leaflet -----------------------------------------------------
  
  
  # show map using googleVis
  palColor = colorFactor(c("blue", "red", "green", "yellow" , "grey"), 
                          domain = c("1 bed", "2 bed", "3 bed" , "3+ bed","Null"))
  
  output$map =renderLeaflet({
    
      leaflet(selected_df()) %>%
      addTiles() %>%  
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      addCircleMarkers(
        # data =IndividualApartment2 , 
        # lat = ~ latitude, lng = ~ longitude, 
        # group  = NoOfBedroom,
        radius = 6, 
        stroke = FALSE, 
        fillOpacity  =  1,
        fillColor =  ~ palColor(selected_df()$Bedroom_number),
        clusterOptions = markerClusterOptions(disableClusteringAtZoom = 15,
                                              # animateAddingMarkers= TRUE,
                                              animate = TRUE),
        popup = ~ paste( sep = "<br/>",
                         "<b><a href='http://www.samurainoodle.com'>Description</a></b>",
                         paste('<b>Apartment : </b>', Aptname,sep = ' '  ),
                         paste('<b>Number of Bedroom :</b>', NoOfBedroom, sep = ' '),
                         paste('<b>Number of Bathroom :</b>',  NoOfBathRoom, sep = ' '),
                         paste('<b>Rent($) :</b>',  RentRate, sep = ' '),
                         paste('<b>ZipCode($) :</b>', Postal , sep = ' '),
                         paste('<b>Elementary School Rating :</b>',  ElementarySchool_rating, 
                               sep = ' '),
                         paste('<b>Middle School Rating :</b>',  MiddleSchool_rating, sep = ' '),
                         paste('<b>High School Rating :</b>',  HighSchool_rating, sep = ' '),
                         paste('<b>Crime Level :</b>',  CrimeLevel, sep = ' ')
        )) %>%
      setView(lng = -74,
              lat = 40.8,
              zoom = 12)   %>%
      addLegend("topleft", pal = palColor,
                values = ~ selected_df()$Bedroom_number,
                title = "Type of Rental",
                opacity = 0.5   )
    
    # m  
    
  })
  
  
  #--------------------------------Leaflet----------------------------------------------------------
  # plot inside Leaflet
  
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
            group_by(Bedroom_number) %>%
            summarise(Total = n())
          
        
    
    # gvisColumnChart(data = df_plot2,
    #                 xvar = "Bedroom_number", 
    #                 yvar = "Total",
    #                 options=list(
    #                   # chartArea=  "{left:110,top:50,width:'80%',height:'55%'}",
    #                   # seriesType="bars",
    #                   # legend ="top",
    #                   # title="Type of Rental Apartments in New York Neighborhood ",
    #                   # height = 290,
    #                   # width = 1180,
    #                   hAxis= "{title: 'Neighborhoond'}",
    #                   vAxis= "{title: 'Frequency'}"
    #                 ))
    
    color1 = c("blue","red", "green", "yellow" ,"grey")
    
    my_func = function(x){
      if(x=='1 bed') return ('1')
      else if (x=='2 bed') return ('2')
      else if (x=='3 bed') return ('3')
      else if (x=='3+ bed') return ('3+ bed')
      else  return ('Null')
    }
    
    names(color1) = c('1','2','3','3+ bed','Null')
    
    if(input$Bedrooms=='All'){
      color1 = color1[sapply(unique(df_plot2$Bedroom_number), my_func)]
    }
    else if (input$Bedrooms=='4' | input$Bedrooms=='5' |input$Bedrooms=='6'| input$Bedrooms=='7') {
      
      color1 = color1["3+ bed"]
    }
    else{
      color1 = color1[input$Bedrooms]
      }
    print(names(color1))
    print(unique(df_plot2$Bedroom_number))
    g = ggplot(data=df_plot2, 
                aes(x=factor(Bedroom_number),y=Total))
    g = g + geom_bar(stat='identity', fill=color1)
    g = g + xlab('Type of Apartment')
    g = g + ylab('Frequency')
    g = g + theme_minimal()
    g
    
    
       
    
  })
  
  
  
  
  #----------------------------------Datatable _----------------------------------------------------
  
  # show data using DataTable
  output$table = DT::renderDataTable({
    
    datatable( #IndividualApartment2,
              selected_df()[,  c(2,3,4,5,6,8,9,11,14,15,16,17,18)],
                           rownames=FALSE) %>%
              formatStyle(input$selected,
                           background="skyblue", fontWeight='bold')
    
    # datatable22()
    
    
    
  })
  
  
  #----------------------------------Barplots ---------------------------------------------
  
  #boxplot
  
  barPlotType1= reactive({
    return (input$SelectBarplot1)
  })
  
  
  output$plot1 = renderChart({
    if (barPlotType1() == "1"){
    
      n1 = nPlot(TotalApartments ~ Neighborhood, group = "Bedroom_number",
                  data = Apt_Bedrooms_rent_Neighborhood,
                  type = "multiBarChart")
      n1$xAxis(rotateLabels=-90)
      n1$chart(reduceXTicks = FALSE) 
      n1$addParams(dom = 'plot1')
      
      return (n1)
    }
    else {
      n1 = nPlot(MedianRent ~ Neighborhood, group = "Bedroom_number",
                  data = Apt_Bedrooms_rent_Neighborhood,
                  type = "multiBarChart")
      n1$xAxis(rotateLabels=-90)
      n1$chart(reduceXTicks = FALSE) 
      n1$addParams(dom = 'plot1')
      
      
      return (n1)
      
    }
    
  })
  
  
  #---------------------------------------------------------------------------------------------
  # 
  # output$plot3 = renderGvis({
  #      gvisColumnChart(data = Apt_Bedrooms_rent_Neighborhood_3,
  #                             xvar = "Neighborhood",
  #                             yvar = c("1", "2","3","3+"),
  #                             options=list(
  #                               chartArea=  "{left:110,top:50,width:'80%',height:'55%'}",
  #                               seriesType="bars",
  #                               legend ="top",
  #                               title="Rental Price by Apartment Types(number of bedrooms) in Manhattan  Neighborhood ",
  #                               height = 290,
  #                               width = 1180,
  #                               hAxis= "{title: 'Neighborhoond'}",
  #                               vAxis= "{title: 'RentPrice'}"
  #                             ))
  # 
  # })


  #--------------------------------------------------------------------------------------------------

  
             # gvisColumnChart(data = Apt_Crime_Neighborhood_2,
             #                  xvar = "Neighborhood",
             #                  yvar = c("Low", "Lowest","High","Highest"),
             #                  options=list(
             #                    chartArea=  "{left:110,top:130,width:'77%',height:'55%'}",
             #                    seriesType="bars",
             #                    legend ="top",
             #                    title="Crime Level near Apartments in Manhattan  Neighborhood ",
             #                    height = 500,
             #                    width = 1180,
             #                    hAxis= "{title: 'Neighborhoond'}",
             #                    vAxis= "{title: 'Frequency'}"
             #                  ))
        output$plot2 = renderChart({
          n1 = nPlot(frequency ~ Neighborhood, group = "CrimeLevel",
                      data = Apt_Crime_Neighborhood,
                      type = "multiBarChart")
          n1$xAxis(rotateLabels=-90)
          n1$chart(reduceXTicks = FALSE) 
          n1$addParams(dom = 'plot2')
         
   
          return (n1)
    
    
  })



  #----------------------------------------------------------------------------------------------

  barPlotType2= reactive({
    return (input$SelectBarplot2)
  })
        
        
        
  output$plot3 = renderChart({
    
    
    if (barPlotType2() == "1"){
      
      n1 = nPlot(frequency ~ Neighborhood, group = "ElementarySchool_rating",
                  data = Apt_ElementarySchool_Neighborhood,
                  type = "multiBarChart")
      n1$xAxis(rotateLabels=-90)
      n1$chart(reduceXTicks = FALSE) 
      n1$addParams(dom = 'plot3')
     
      
      return (n1)
    }
    else if (barPlotType2() == "2") {
      
      n1 = nPlot(frequency ~ Neighborhood, group = "MiddleSchool_rating",
                  data = Apt_MiddleSchool_Neighborhood,
                  type = "multiBarChart")
      n1$xAxis(rotateLabels=-90)
      n1$chart(reduceXTicks = FALSE) 
      n1$addParams(dom = 'plot3')
      
      
      
      return (n1)
      
    }
    else{
      
      n1 = nPlot(frequency ~ Neighborhood, group = "HighSchool_rating",
                  data = Apt_HighSchool_Neighborhood,
                  type = "multiBarChart")
      n1$xAxis(rotateLabels=-90)
      n1$chart(reduceXTicks = FALSE) 
      n1$addParams(dom = 'plot3')
      
      
      return (n1)
      
    }
    
    
  })


  # #---------------------------------------------------------------------------------------------------

  output$plot6 = renderGvis({
           gvisColumnChart(data = Apt_MiddleSchool_Neighborhood_2,
                              xvar = "Neighborhood",
                              yvar = c( "Above Average","Average", "Below Average","Not Mentioned"),
                              options=list(
                                chartArea=  "{left:110,top:50,width:'75%',height:'50%'}",
                                seriesType="bars",
                                legend ="top",
                                title="Middle School Ratings near Apartments in Manhattan Neighborhood ",
                                height = 220,
                                width = 1180,
                                hAxis= "{title: 'Neighborhoond'}",
                                vAxis= "{title: 'Frequency'}"
                              ))


  })

 # ----------------------------------------------------------------------------------------------------
  
  output$plot7 = renderGvis({
           gvisColumnChart(data = Apt_HighSchool_Neighborhood_2,
                              xvar = "Neighborhood",
                              yvar = c( "Above Average","Average", "Below Average","Not Mentioned"),
                              options=list(
                                chartArea=  "{left:110,top:50,width:'75%',height:'50%'}",
                                seriesType="bars",
                                legend ="top",
                                title="High School Ratings near Apartments in Manhattan Neighborhood ",
                                height = 220,
                                width = 1180,
                                hAxis= "{title: 'Neighborhoond'}",
                                vAxis= "{title: 'Frequency'}"
                              ))

  })

  #-----------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   
})