shinyServer(function(input, output) {
  
  data_wd <- reactive({
    return(world)
  })
  
  output$plot_wd <- renderGvis({
    w = gvisTable(data_wd(), option = list(width=600, height=550))
    return(w)
  })
  
  output$plot_wdh <- renderGvis({
    h = gvisColumnChart(data_wd(), option = list(vAxis="{title:'Number of People'}",
                                                 hAxis="{title:'Region'}", width=750, height=550))
    return(h)
  })
  data_tn <- reactive({
    if(input$radio == "nam")
      return(nam)
    if(input$radio == "top")
      return(top)
  })
  
  output$plot_tn <- renderGvis({
    g = gvisGeoChart(data_tn(),'country', 'value')
    return(g)
  })
  
  
  data_bp <- reactive({
    return(p2)
  })
  
  output$plot_bp <- renderGvis({
    f = gvisColumnChart(p2, option = list(vAxis="{title:'Number of People'}",
                                          hAxis="{title:'Country'}", width=1150, height=550))
    return(f)
  })
  
  
  data_ie <- reactive({
    print(input$dates[1])
    df = z[z$Date >= as.Date(input$dates[1]) & z$Date <= as.Date(input$dates[2]), ]
    
    df = df[, c(as.integer(input$select), 7, 8)]
    names(df) <- c("value", "Date", "Trade")
    return (df)
  })
  
  output$plot_ie <- renderPlot({
    ggplot(data_ie(),aes(x = Date, y = value)) + geom_line(aes(color= Trade)) + labs(title='Import vs Export of United States')
    
  }, height = 500, width = 600)
  
  data_tp <- reactive({
    return(topports1)
  })
  
  output$plot_tp <- renderGvis({
    p = gvisPieChart(data_tp(),option = list(width=750, height=550))
    return(p)
  })
})