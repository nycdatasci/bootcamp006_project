library(shiny)
library(plotly)
library(dplyr)
library(countrycode)

shinyServer(function(input, output) {
  
  
  chooseMap=function(a){
    switch(a, 
           "mug_country"={return(mug_country)},
           "mug_country_noUSA"={return(mug_country_noUSA)},
           "user_country"={return(user_country)},
           "user_country_noUSA"={return(user_country_noUSA)})
  }
  
  output$trendPlot <- renderPlotly({
    plot_ly(chooseMap(input$geotype), z = Quantity, text = Country, 
            locations = CountryCode, type = 'choropleth',
            color = Quantity, colors = 'GnBu', marker = list(line = l),
            colorbar = list(title = 'Counts')) %>%
      layout(geo = g)
  })
  
  output$kmeans <- renderPlotly({
    plot_ly(data = mug, x = Owner, y = Seeker, mode = "markers",
            text= paste(Name, "<br>Edition: ", Edition, "<br>Country: ", Country,
                        "<br>City: ",City, "<br>Owner: ",Owner,"<br>Seeker: ",
                        Seeker,"<br>Trader",Trader, "<br>Difficulty: ",Difficulty),
            color = mugCluster$cluster, 
            colors =c('olivedrab3','navyblue','indianred2','darkgoldenrod1')) %>% 
      layout(title='K-means Clustering: # of Seekers vs. # of Owners')
  })
  
  output$plotedition <- renderPlotly({
    plot_ly(data = popular_editions, x = Rarity, y = Popularity, mode = "markers",
            text= paste(Name, "<br>Edition: ", Edition, "<br>Country: ", Country,
                        "<br>City: ",City, "<br>Owner: ",Owner,"<br>Seeker: ",
                        Seeker,"<br>Trader: ",Trader, "<br>Difficulty: ",Difficulty),
            color = Edition, 
            colors = c('olivedrab3','navyblue','indianred2','darkgoldenrod1')) %>%
      layout(title='Collectible Editions: Popularity vs. Rarity')
  })
      
  output$plotcountry <- renderPlotly({
    plot_ly(data = popular_country, x = Rarity, y = Popularity, mode = "markers",
            text= paste(Name, "<br>Edition: ", Edition, "<br>Country: ", Country,
                        "<br>City: ",City, "<br>Owner: ",Owner,"<br>Seeker: ",
                        Seeker,"<br>Trader: ",Trader, "<br>Difficulty: ",Difficulty),
            color = Country, 
            colors = 'Dark2') %>%
      layout(title='Collectible Countries: Popularity vs. Rarity')
  })
  
  
})