

shinyServer(function(input, output){
  
  # 
  output$marital_motion <- renderGvis({
    gvisMotionChart(marital_data, 'Marital_Status', 'Year')
  })
  
  # 
  output$gender_motion <- renderGvis({
    gvisMotionChart(gender_data, 'Gender', 'Year')
  })
  
  # 
  output$race_motion <- renderGvis({
    gvisMotionChart(race_data, 'Race', 'Year')
  })
  
  #
  output$race_gender_motion <- renderGvis({
    gvisMotionChart(race_gender_data, 'Race', 'Year')
  })
  
  # show histogram using googleVis
  output$hist <- renderGvis({
    gvisColumnChart(data = filter(hist_data,
                                  Marital_Status == input$marital_status,
                                  Year == input$year,
                                  Race == input$race,
                                  Gender == input$gender)[, !(colnames(hist_data) %in% optional_columns), drop=FALSE]
    )
  })
})




