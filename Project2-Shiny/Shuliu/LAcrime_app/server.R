library(DT)
library(shiny)
library(googleVis)
library(leaflet)
library(graphics)

shinyServer(function(input, output){
  
  # show map using ledflet
  #crime_map <- filter(crimedf2, Year_occ == c('input$CMyear'))
  
  #
  crime_map <- reactive({
    if (input$CMarea == 1) {
      filter(crimedf2, Year_occ %in% input$CMyear)
    }else {
      filter(crimedf2, Year_occ %in% input$CMyear, AREA.NAME %in% input$CMarea)
    }
  })
  
  output$mymap <- renderLeaflet({
    
    #print('test')
    leaflet(crime_map()) %>%
      addTiles() %>%  # Add default OpenStreetMap map titles
      addMarkers(lat = crime_map()$locx, lng = crime_map()$locy,
                 clusterOptions = markerClusterOptions())
  })

  # show daily crime using googleCal
  output$calendar <- renderGvis(
    gvisCalendar(dycount_df, 
                 datevar="DATE.OCC1", 
                 numvar="cnt",
                 options=list(
                   title="Daily Crimes in Los Angeles",
                   height=620,
                   calendar="{yearLabel: { fontName: 'Times-Roman',
                        fontSize: 32, color: '#1A8763', bold: true},
                        cellSize: 10,
                        cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                        focusedCellColor: {stroke:'green'}}")
    )
    
  )
  
  # show crime areas using bar chart
  #print(length(unique(crime_map()$AREA.NAME)))
  
  output$crm_area <- renderGvis({
    if(length(unique(crime_map()$AREA.NAME)) != 1){  #  'All areas' is selected by user
      gvisBarChart(areaTOTcount_df,
                   options = list(
                     title = 'Number of Crimes in LAPD Areas',
                     width = '600px', 
                     height = '800px',
                     legend = "{position: 'top'}")
      )
    } else {
      areaSEPcount_df <- crime_map() %>%
        group_by(Year_occ) %>%
        summarise(volume = n())
      
      
      gvisLineChart(areaSEPcount_df,
                    options = list(
                      title='Volume Changes of Crimes from 2011 to 2015',
                      colors = 'red',
                      width = '520px',
                      height = '400px',
                      legend = "{position: 'top'}")
      )
    }
  })
  
  
  # Show total volume changes of crimes using line chart
  output$crm_tot_chg <- renderGvis({
    gvisLineChart(TOTcount_df,
                  options = list(
                    title='Total Volume Changes of Crimes from 2011 to 2015',
                    width = '520px',
                    height = '400px',
                    legend = "{position: 'top'}")
    )
  })
  
  # Show crime status using pie chart
  output$crm_status <- renderGvis(
    if(length(unique(crime_map()$AREA.NAME)) != 1){
      gvisPieChart(statcount_df,
                   options = list(
                     slices="{3: {offset: 0.3}, 4: {offset: 0.2}}",
                     title='Status of Crimes',
                     pieSliceText='label',
                     height = '600',
                     width = '800')
      )
    }
  )
  
  # Show crimes data table
  output$crm_table <- renderDataTable({
    dttable_df
  })
  
  # show crime status in days using stacked bar charts
  output$crm_days_status <- renderGvis({
    gvisBarChart(daycount_df, xvar = 'Day_occ', 
                 yvar = c('Adult Arrest', 'Adult Other', 'Invest Cont', 'Juv Arrest', 'Juv Other', 'Unknown'),
                 options = list(title = 'Crime Status in Days',
                                legend = 'bottom',
                                height = 800,
                                width = 800,
                                isStacked = TRUE,
                                bar="{groupWidth:'50%'}"))
  })
  
}
)
