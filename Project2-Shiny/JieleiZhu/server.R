
shinyServer(function(input, output){
  
  # show income data for never married, married(total) and divorced
  output$marital_total_motion <-renderGvis({
    gvisMotionChart(marital_total, 'Marital_Status', 'Year')
  })
  
  # show income data for everything above plus spouse present and spouse absent
  output$marital_motion <- renderGvis({
    gvisMotionChart(marital_data, 'Marital_Status', 'Year')
  })
  
  # show income data for female and male
  output$gender_motion <- renderGvis({
    gvisMotionChart(gender_data, 'Gender', 'Year')
  })
  
  # show income data for white, black, asian, and hispanic
  output$race_motion <- renderGvis({
    gvisMotionChart(race_data, 'Race', 'Year')
  })
  
  # show income data for white female, white male, black female, black male...
  output$race_gender_motion <- renderGvis({
    gvisMotionChart(race_gender_data, 'Race', 'Year')
  })
  
  # show histogram using googleVis
  data <-reactive({
    data = rename(melt(filter(hist_data,
                  Marital_Status == input$marital_status,
                  Year == input$year,
                  Race == input$race,
                  Gender == input$gender)[, !(colnames(hist_data) %in% optional_columns), drop=FALSE]
  ))}) #, 'Population (in thousands)'=value)})
  
  output$hist <- renderGvis({
    (a<- gvisColumnChart(data(), 
                         options=list(fontSize = 16,
                                      hAxis = "{title : 'Income categories'}",
                                      vAxis = "{title : 'Population (in thousands)'}",
                                      legend = "{position:'none'}",
                                      width = 1200,
                                      height = 700)))
  })
})


