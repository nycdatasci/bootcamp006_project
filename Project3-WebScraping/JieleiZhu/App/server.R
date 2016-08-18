shinyServer(function(input, output, session){
  output$hist <- renderGvis({
    (a<- gvisColumnChart(data(), 
                         options=list(fontSize = 16,
                                      hAxis = "{title : 'Income categories'}",
                                      vAxis = "{title : 'Population (in thousands)'}",
                                      legend = "{position:'none'}",
                                      width = 1200,
                                      height = 700)))
  })


  popular_data = reactive({
    if (input$PopularMetric == 'Both') {
      popular_data = without_duplicates %>% select(Category, Title, Description, Popularity = Popularity) %>% filter(Category == input$PopularCategory) 
    }
    else if(input$PopularMetric == 'By Comments') {
      popular_data = without_duplicates %>% select(Category, Title, Description, Popularity = NumberOfComments) %>% filter(Category == input$PopularCategory) 
    }
    else if(input$PopularMetric == 'By Bookmarks') {
      popular_data = without_duplicates %>% select(Category, Title, Description, Popularity = NumberOfBookmarks) %>% filter(Category == input$PopularCategory) 
    }
    
    Popularity = popular_data$Popularity
    return(popular_data[order(Popularity, decreasing = T), ])
  })
  
  
  
  output$TopDeal = renderDataTable({
    popular_data()
  }, options = list(lengthMenu = c(5, 10, 15, 20), pageLength = 10))
  
  
  output$PopularStore = renderDataTable({
    temp = master_popularity_data %>% filter(Category == input$PopularStoreCategory) %>% filter(NumDeals >= input$MinimumDeals) %>% select(Category, Store, Popularity = PPD) 
    PPD = temp$Popularity
    return(temp[order(PPD, decreasing = T), ])
    
  }, options = list(lengthMenu = c(5, 10, 15, 20), pageLength = 10))
  
  
  output$MostStore = renderDataTable({
    most_store_temp = most_store %>% filter(Category == input$MostStoreCategory)

    NumDeals = most_store_temp$NumberOfDeals
    return(most_store_temp[order(NumDeals, decreasing = T), ])
  }, options = list(lengthMenu = c(5, 10, 15, 20), pageLength = 10))
  
  
  output$calendar = renderGvis({
    month_data = without_duplicates %>% 
      filter(Category == input$MostDealCategory, year(PostedTime) > 2012) %>% 
      group_by(PostedTime, Category) %>%  
      dplyr::summarise(Count = n())
    
    gvisCalendar(month_data,
                 datevar = "PostedTime",
                 numvar = "Count",
                 options = list(
                   # title = "Monthly Frequency of Deals",
                   height = "500px",
                   calendar = "{yearLabel: {fontName: 'Times-Roman',
                          fontSize: 32, color: '#1A8763', bold: true},
                          cellSize: 10,
                          cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                          focusedCellColor: {stroke:'red'}}"
                   )
    )
  })
  

  output$annotation = renderGvis({
    temp = without_duplicates %>% 
      filter(Category == input$SingleMostCategory, Store == input$SingleMostStore) %>% 
      group_by(PostedTime, Category) %>% 
      dplyr::summarise(Count = n())
    
    gvisAnnotationChart(temp,
                        datevar="PostedTime",
                        numvar="Count",
                        options=list(
                          width=600, height=350,
                          fill=10, displayExactValues=TRUE,
                          colors="['#0000ff']")
    )
    
  })
})