shinyServer(function(input, output){
  output$hist <- renderGvis({
    (a<- gvisColumnChart(data(), 
                         options=list(fontSize = 16,
                                      hAxis = "{title : 'Income categories'}",
                                      vAxis = "{title : 'Population (in thousands)'}",
                                      legend = "{position:'none'}",
                                      width = 1200,
                                      height = 700)))
  })
  
<<<<<<< HEAD
  
  

=======
  popular_data = reactive({
    if (input$PopularMetric == 'Both') {
      popular_data = without_duplicates %>% select(Category, Title, Description, Popularity = Popularity) %>% filter(Category == input$PopularCategory) 
    }
    else if(input$PopularMetric == 'Comments') {
      popular_data = without_duplicates %>% select(Category, Title, Description, Popularity = NumberOfComments) %>% filter(Category == input$PopularCategory) 
    }
    else if(input$PopularMetric == 'Bookmarks') {
      popular_data = without_duplicates %>% select(Category, Title, Description, Popularity = NumberOfBookmarks) %>% filter(Category == input$PopularCategory) 
    }
    
    Popularity = popular_data$Popularity
    return(popular_data[order(Popularity, decreasing = T), ])
  })
  
  
  
  output$TopDeal = renderDataTable({
    popular_data()
  }, options = list(lengthMenu = c(5, 10, 15, 20), pageLength = 5))
  
  
  output$PopularStore = renderDataTable({
    temp = master_popularity_data %>% filter(Category == input$PopularStoreCategory) %>% filter(NumDeals >= input$MinimumDeals) %>% select(Category, Store, Popularity = PPD) 
    PPD = temp$Popularity
    return(temp[order(PPD, decreasing = T), ])
    
  }, options = list(lengthMenu = c(5, 10, 15, 20), pageLength = 5))
  
  
  output$MostStore = renderDataTable({
    NumDeals = most_store$NumberOfDeals
    return(most_store[order(NumDeals, decreasing = T), ])
  }, options = list(lengthMenu = c(5, 10, 15, 20), pageLength = 5))
  
  
  # output$timeline = renderGvis({
  #   month_data = dplyr::summarise(group_by(filter(without_duplicates, Category == input$MostDealCategory), PostedTime, Category), Count = n())
  #   
  #   gvisCalendar(month_data, 
  #                datevar = "PostedTime", 
  #                numvar = "Count",
  #                options = list(
  #                  title = "Monthly Frequency of Deals",
  #                  height = 320,
  #                  calendar = "{yearLabel: { fontName: 'Times-Roman',
  #                         fontSize: 32, color: '#1A8763', bold: true},
  #                         cellSize: 10,
  #                         cellColor: { stroke: 'red', strokeOpacity: 0.2 },
  #                         focusedCellColor: {stroke:'red'}}")
  #   )
  # })
  
  # output$annotation = renderGvis({
  # temp = without_duplicates %>% dplyr::filter(Category == input$SingleMostCategory, Store == input$DingleMostStore) %>% dplyr::group_by(PostedTime, Category) %>% dplyr::summarise(Count = n())
  # 
  # 
  #   gvisAnnotationChart(temp,
  #                       datevar="PostedTime",
  #                       numvar="Count",
  #                       # idvar="Device",
  #                       # titlevar="Title",
  #                       # annotationvar="Annotation",
  #                       options=list(
  #                         width=600, height=350,
  #                         fill=10, displayExactValues=TRUE,
  #                         colors="['#0000ff']")
  #   )
  # })

  
>>>>>>> d3eae80b1667a0750991df3a7dcd9151a194eac1
})



