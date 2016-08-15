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
  
  
  

})



