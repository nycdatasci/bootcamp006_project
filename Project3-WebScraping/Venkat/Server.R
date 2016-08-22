#Server
library(leaflet)
library(dplyr)
library(SnowballC)
library(stringr)
library(stringi)

shinyServer(function(input, output) {
  
  data_wd <- reactive({
    return(df1)
  })
  
  output$plot_rm <- renderLeaflet({
    m <- leaflet() %>% addTiles() %>% addMarkers( data_wd()$long, data_wd()$lat, popup = data_wd()$NAME) %>% setView(lng = -73.9851, lat = 40.7589,zoom = 15)
    return(m)
  })
  
  
  data_ie <- reactive({
    return (df1)
  })
  
  output$plot_tr <- renderPlot({
    ggplot(data_ie(), aes(data_ie()$RATING)) + geom_histogram() + xlab("Rating") + ggtitle("Distribution Of Rating")
  })
  
  data_io <- reactive({
    return (df1)
  })
  
  output$plot_rd <- renderPlot({
    ggplot(data_io(), aes(data_io()$REVIEWS)) + geom_histogram() + xlab("Number Of Reviews") + ggtitle("Distribution Of Number Of Reviews")
  })
  
  data_sa <- reactive({
    g = filter(data, NAME==input$select2)
    r = stack(select(g, POSITIVE, NEGATIVE))
    return(r)
  })
  
  output$plot_tt <- renderPlot({
    ggplot(data_sa())+geom_bar(aes(x=ind, y=values), stat='identity')
  })
  
  data_mm <- reactive({
    g = filter(data, NAME==input$select2)
    return(g)
  })
  output$plot_ta <- renderGvis({
    Table <- gvisTable(data_mm()[,3:7],option = list(width=600, height=150))
    return(Table)
  })
  
  
  data_wc <- reactive({
    w = filter(output2, NAME==input$select1)
    return(w)
  })
  output$plot_wc <- renderPlot({
    wordcloud(getTextData(data_wc())
    )
  })
})



