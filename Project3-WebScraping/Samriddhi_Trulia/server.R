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
                          domain = c("1", "2", "3" , "3+","Null"))
  
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
                         paste('Apartment : ', Aptname,sep = ' '  ),
                         paste('Number of Bedroom :', NoOfBedroom, sep = ' '),
                         paste('Number of Bathroom :',  NoOfBathRoom, sep = ' '),
                         paste('Rent($) :',  RentRate, sep = ' '),
                         paste('Elementary School Rating :',  ElementarySchool_rating, 
                               sep = ' '),
                         paste('Middle School Rating :',  MiddleSchool_rating, sep = ' '),
                         paste('High School Rating :',  HighSchool_rating, sep = ' '),
                         paste('Crime Level :',  CrimeLevel, sep = ' ')
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
  
  
  
  
  
  output$barplot1 = renderGvis({
    
          df_plot =selected_df()
          df_plot2 = df_plot %>%
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
          
          
          g = ggplot(data=df_plot2, 
                      aes(x=Bedroom_number,y=Total))
          g = g + geom_bar(stat='identity', fill = 'purple4')
          g = g + xlab('Type Of Apartment')
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
    # Highlight selected column using formatStyle
  })
  
  
  #----------------------------------Barplots ---------------------------------------------
  
  output$plot2 = renderGvis({
    gvisColumnChart(data = Apt_Bedrooms_rent_Neighborhood_2, 
                    xvar = "Neighborhood", 
                    yvar = c("1", "2","3","3+"),
                    options=list(
                      chartArea=  "{left:110,top:50,width:'80%',height:'55%'}",
                      seriesType="bars",
                      legend ="top", 
                      title="Type of Rental Apartments (number of bedroom) in Manhattan Neighborhood ",
                      height = 290,
                      width = 1180, 
                      hAxis= "{title: 'Neighborhoond'}", 
                      vAxis= "{title: 'Frequency'}"
                    ))
    
  })
  
  
  #---------------------------------------------------------------------------------------------
  
  output$plot3 = renderGvis({
       gvisColumnChart(data = Apt_Bedrooms_rent_Neighborhood_3,
                              xvar = "Neighborhood",
                              yvar = c("1", "2","3","3+"),
                              options=list(
                                chartArea=  "{left:110,top:50,width:'80%',height:'55%'}",
                                seriesType="bars",
                                legend ="top",
                                title="Rental Price by Apartment Types(number of bedrooms) in Manhattan  Neighborhood ",
                                height = 290,
                                width = 1180,
                                hAxis= "{title: 'Neighborhoond'}",
                                vAxis= "{title: 'RentPrice'}"
                              ))

  })


  #--------------------------------------------------------------------------------------------------

  output$plot4 = renderGvis({
             gvisColumnChart(data = Apt_Crime_Neighborhood_2,
                              xvar = "Neighborhood",
                              yvar = c("Low", "Lowest","High","Highest"),
                              options=list(
                                chartArea=  "{left:110,top:130,width:'77%',height:'55%'}",
                                seriesType="bars",
                                legend ="top",
                                title="Crime Level near Apartments in Manhattan  Neighborhood ",
                                height = 500,
                                width = 1180,
                                hAxis= "{title: 'Neighborhoond'}",
                                vAxis= "{title: 'Frequency'}"
                              ))


  })



  #----------------------------------------------------------------------------------------------

  output$plot5 = renderGvis({
              gvisColumnChart(data = Apt_ElementarySchool_Neighborhood_2,
                              xvar = "Neighborhood",
                              yvar = c( "Above Average","Average", "Below Average","Not Mentioned"),
                              options=list(
                                chartArea=  "{left:110,top:50,width:'75%',height:'50%'}",
                                seriesType="bars",
                                legend ="top",
                                title="Elementary School Ratings near Apartments in Manhattan Neighborhood ",
                                height = 220,
                                width = 1180,
                                hAxis= "{title: 'Neighborhoond'}",
                                vAxis= "{title: 'Frequency'}"
                              ))


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