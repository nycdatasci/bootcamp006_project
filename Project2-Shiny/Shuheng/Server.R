library(shiny)
library(threejs)
library(data.table)
library(dplyr)
library(ggplot2)


investment <- fread('investments.csv', na.strings = c('','-'))
investment <- as.data.frame(investment)
invest <- read.csv("./invest_globe1.csv")
flow_in <- read.csv("./flow_in.csv")
flow_out <- read.csv("./flow_out.csv")
domestic <- read.csv("./domestic.csv")

earth <- system.file('images/world.jpg',  package="threejs")
#ggplot2 function
gg_output <- function(df, by){
  switch(by,
         Total = ggplot(data = df, aes(x = long, y = lat)) + 
           geom_polygon(aes(group = group, fill = sum)),
         Average = ggplot(data = df, aes(x = long, y = lat)) + 
           geom_polygon(aes(group = group, fill = mean))
  )
}

#funding breakdown function
funding_breakdown <- function(df,company)
{
  breakdown_table <- subset(df,company_name == company) %>%
    group_by(funding_round_code) %>% 
    summarise(funding_sum = sum(raised_amount_usd))
  
  return(breakdown_table)
}

#

shinyServer(function(input, output, ssesion) {
    output$invest_global_map <- renderGlobe({
      invest_3D_use <- invest[1:input$N,]
      
      globejs(img=earth, arcs=invest_3D_use,
              arcsHeight=0.3, arcsLwd=2,
              arcsColor='#E73009', arcsOpacity=0.3,atmosphere=T, 
              bg = '0f1119')
    })
    
    output$plot_in <- renderPlot({
      gg_output(flow_in, input$in_by)
      })
    
    output$plot_out <- renderPlot({
      gg_output(flow_out, input$out_by)
    })
    
    output$plot_do <- renderPlot({
      gg_output(domestic, input$do_by)
    })
    
    output$breakdown <- renderDataTable({
      funding_breakdown(investment,input$company)
    })
    
    output$table <- renderDataTable({
      investment
    })
    

})