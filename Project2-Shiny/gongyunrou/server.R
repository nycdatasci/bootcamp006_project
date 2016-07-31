library(shiny)
library(googleVis)
library(dplyr)
library(stringr)
library(DT)
shinyServer(function(input, output){
  
    # show motion chart1

    output$motion1 <- renderGvis({
        gvisMotionChart(year,
                        idvar="Airport", timevar ="Year",
                        options=list(width="auto", height="auto"))
    
    })
    
    #motion chart 2
    data_motion_month<- reactive({
      data_month <- month %>% filter(.,Year == input$Year_motion2)
    })
    
    output$motion2 <-renderGvis({
        gvisMotionChart(data_motion_month(),
                        #filter(month,Year==1999),
                        idvar="Airport",
                        timevar = "Month",
                        xvar="Occupancy", yvar="Flights",
                        colorvar="AirportType", sizevar="Population",
                        options=list(width="auto", height="auto"))

    })
    
    # show Airport Location Map
    output$airport <- renderGvis({
        gvisGeoChart(filter(state_stat,Year==1990 & AirportType=="SmallHub"),
                     locationvar ="latlong",
                     colorvar = "Occupancy",
                     sizevar="Flights",
                     hovervar = "Airport",
                     options=list(displayMode="Markers", region="US",
                                  colorAxis="{minValue:0,maxValue:1,colors:['#FAF2FC', '#9900CC']}",
                                  backgroundColor="lightblue",
                                  width="auto", height="auto"))
      
    })
    
    #show state map using googleVis
    data_map<- reactive({
      data_map_filter<- data %>% filter(.,Year == input$Year_map & 
                                      Month ==input$Month_map)
    })
    
    
    output$map<- renderGvis({
      gvisGeoChart(data_map()%>%
          group_by(State)%>%
          summarise(Netflow=sum(Netflow)),
        locationvar ="State",
        colorvar = "Netflow",
        options=list(region="US",
                     dataMode="regions",
                     resolution="provinces",
                     colorAxis="{minValue:-450000,maxValue:450000,colors: ['#CC3300','#FFFFFF','green']}",
                     width=800, height=600,
                     title="Netflow of Passengers in JAN.1990 by State",
                     titleTextStyle="{color:'#003366',fontName:'Arial',
                     fontSize:18}",
                     legend="{ position:'bottom'}"))
      
      
      
    })
    
    
    
    # # show histogram using googleVis
    # output$hist <- renderGvis({
    #     gvisHistogram(state_stat[,input$selected, drop=FALSE])
    # })
    
    # show data using DataTable
    output$table <- DT::renderDataTable({
        datatable(data, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    # show statistics using infoBox
    output$maxBox <- renderInfoBox({
        max_value <- max(data_map()%>%
                           group_by(State)%>%
                           summarise(Netflow=sum(Netflow)))
        # max_state <- 
        #     data_map()%>%group_by(State)%>%
        #   summarise(Netflow=sum(Netflow))%>%
        #               find(State== max_value)
        # infoBox(max_state, max_value, icon = icon("hand-o-up"))
    })
    # output$minBox <- renderInfoBox({
    #     min_value <- min(state_stat[,input$selected])
    #     min_state <- 
    #         state_stat$state.name[state_stat[,input$selected] == min_value]
    #     infoBox(min_state, min_value, icon = icon("hand-o-down"))
    # })
    # output$avgBox <- renderInfoBox(
    #     infoBox(paste("AVG.", input$selected),
    #             mean(state_stat[,input$selected]), 
    #             icon = icon("calculator"), fill = TRUE))
})