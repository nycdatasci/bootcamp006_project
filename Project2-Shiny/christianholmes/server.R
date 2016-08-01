shinyServer(function(input, output) {
  graphInput = reactive({
    if (input$lineG == "total") {
      line = total_fatal
    }
    if (input$lineG == 'drunk') {
      line = dd_fatal
    }
    if (input$lineG == 'distract') {
      line = distract_fatal
    }
    if (input$lineG == 'speeding') {
      line = speeding_fatal
    }
    line = line[line$YEAR >= input$lineRange[1] & line$YEAR <= input$lineRange[2],]
    })
  output$trendPlot = renderPlotly({
    
    p = plot_ly(graphInput(), x = YEAR, y = FATALS)
    layout(p, yaxis = list(range = c(0, 50000)),autotick = F)
    
})
    mapInput = reactive({
    if (input$map == "total") {
      mapdat = select(grouped_state, YEAR, Code, rate = total_rate)
    }
    if (input$map == 'drunk') {
      mapdat = select(grouped_state, YEAR, Code, rate = drunk_rate)
    }
    if (input$map == 'distract') {
      mapdat = select(grouped_state, YEAR, Code, rate = distract_rate)
    }
    if (input$map == 'speeding') {
      mapdat = select(grouped_state, YEAR, Code, rate = speeding_rate)
    }
    mapdat = mapdat[mapdat$YEAR >= input$mapRange[1] & mapdat$YEAR <= input$mapRange[2],]
    mapdat = summarise(group_by(mapdat, Code),  rate = mean(rate))
})
  output$map1 = renderPlotly({
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(scope = 'usa',projection = list(type = 'albers usa'),showlakes = TRUE,lakecolor = toRGB('white'))
    
    plot_ly(mapInput(), z = mapInput()$rate, locations = mapInput()$Code, type = 'choropleth', locationmode = 'USA-states', color = rate, colors = 'Purples', marker = list(line = l), colorbar = list(title = "Fatalities per 100,000 Residents")) %>%
      layout(title = '', geo = g)
})
  corrInput = reactive({
    if (input$corr == "total") {
      corrdat = select(grouped_state, YEAR, Code, rate = total_rate)
    }
    if (input$corr == 'drunk') {
      corrdat = select(grouped_state, YEAR, Code, rate = drunk_rate)
    }
    if (input$corr == 'distract') {
      corrdat = select(grouped_state, YEAR, Code, rate = distract_rate)
    }
    if (input$corr == 'speeding') {
      corrdat = select(grouped_state, YEAR, Code, rate = speeding_rate)
    }
    corrdat = corrdat[corrdat$YEAR >= input$corrRange[1] & corrdat$YEAR <= input$corrRange[2],]
    corrdat = summarise(group_by(corrdat, Code),  rate = mean(rate))
  })
  output$corr1 = renderPlotly({
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(scope = 'usa',projection = list(type = 'albers usa'),showlakes = TRUE,lakecolor = toRGB('white'))
    
    plot_ly(corrInput(), z = corrInput()$rate, locations = corrInput()$Code, type = 'choropleth', locationmode = 'USA-states', color = rate, colors = 'Purples', marker = list(line = l), colorbar = list(title = "Fatalities per 100,000 Residents")) %>%
      layout(title = '', geo = g)
    
})  
  barInput = reactive({
    bardat = grouped_state
    state1 = input$stateOne
    state2 = input$stateTwo
    
    bardat = bardat[bardat$STATE == state1 | bardat$STATE == state2,]
    bardat = bardat[bardat$YEAR >= input$barRange[1] & bardat$YEAR <= input$barRange[2],]
    bardat = summarise(group_by(bardat, Code), total_rate = mean(total_rate), drunk_rate = mean(drunk_rate), distract_rate = mean(distract_rate), speeding_rate = mean(speeding_rate))
    if (input$stateradio == "total_rate") {
      bardat = select(bardat, Code, rate = total_rate)
    }
    if (input$stateradio == 'drunk_rate') {
       bardat = select(bardat, Code, rate = drunk_rate)
    }
    if (input$stateradio == 'distract_rate') {
      bardat = select(bardat, Code, rate = distract_rate)
    }
    if (input$stateradio == 'speeding_rate') {
      bardat = select(bardat, Code, rate = speeding_rate)
    }
    bardat
  })
  output$bar_chart = renderPlotly({
    State = barInput()$Code
    Rate = barInput()$rate
    p <- plot_ly(x = State,y = Rate, name = "State Fatality Rate Comparison",type = "bar")
    layout(p, autorange = F,autotick = F, yaxis = list(range = c(0,35)))
  })
})
