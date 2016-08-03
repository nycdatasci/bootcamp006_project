library(DT)
library(shiny)
library(googleVis)
library(ggplot2)

shinyServer(function(input, output){
  # show map using googleVis
  output$map <- renderGvis({
    gvisGeoChart(US_StateStat, "X", input$selected,
                 options=list(region="US", displayMode="regions", 
                              resolution="provinces"#,width="100%", height="100%"
                              ))
  })
  
  
  # show histogram using googleVis
  output$hist <- renderGvis({
    gvisHistogram(US_StateStat[,input$selected, drop=FALSE], options=list(legend="{position: 'bottom'}",width ="600", height= "345"))
  })
  
  # show data using DataTable
  output$table <- DT::renderDataTable({
    datatable(US_StateStat, rownames=FALSE) %>% 
      formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
  
  # show statistics using infoBox
  output$maxBox <- renderInfoBox({
    max_value <- max(US_StateStat[,input$selected])
    max_state <- 
      US_StateStat$state.name[US_StateStat[,input$selected] == max_value]
    infoBox(max_state, max_value, icon = icon("hand-o-up"))
  })
  output$minBox <- renderInfoBox({
    min_value <- min(US_StateStat[,input$selected])
    min_state <- 
      US_Stat$state.name[US_StateStat[,input$selected] == min_value]
    infoBox(min_state, min_value, icon = icon("hand-o-down"))
  })
  output$avgBox <- renderInfoBox(
    infoBox(paste("AVG.", input$selected),
            mean(US_StateStat[,input$selected]), 
            icon = icon("calculator"), fill = TRUE))
  
  output$plot <- renderPlot({
    print (US_Stat)
    #g = ggplot(US_Stat,aes(x=Murder, y = Assault))+ geom_point()
    code = paste0('g = ggplot(US_StateStat,aes(x=', input$choice1, ', y =', input$choice2, '))+ geom_point()' )
    eval(parse(text = code))
    g + geom_smooth(method='lm')
  })
  
})
  
