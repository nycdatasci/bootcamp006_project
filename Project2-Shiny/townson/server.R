library(shinydashboard)
library(googleVis)
library(DT)
library(xts)
library(plotly)

st.codes=readRDS('st.codes.rds')
data_ids=readRDS('data_ids.rds')
other.codes=readRDS('other.codes.rds')
cn.codes=readRDS('cn.codes.rds')
other.con.codes=readRDS('other.con.codes.rds')
total.codes=readRDS('total.codes.rds')
total.con.codes=readRDS('total.con.codes.rds')
source('helpers.R')

shinyServer(function(input,output){
  
  datasetInput <- reactive({
    return(as.character(input$daterange))
  })
  
  # datasetInput2 <- reactive({
  #   return(as.character(input$daterange2))
  # })
  
  datasetInput3 <- reactive({
    return(as.character(input$daterange3))
  })
  
  texasInput <- reactive({
    if (input$texas==TRUE) {
      return(TRUE)
    } else{
      return(FALSE)
    }
  })
  
  second_Cond <-reactive({
    if (input$data_type=='sum'){
      return(1900)
    } else {
      return(as.character(input$daterange3))
    }
  })
  
  timerangeInput <-reactive({
    return(paste0(input$daterange,'/',input$daterange3))
  })
  
  output$filter <- renderUI({
    
    selectizeInput("category","View Withdrawals From:",c('All Sources','Gas Wells',
                                                         'Oil Wells','Shale',
                                                         'Coal Beds'))
    
  })
  output$filter1 <- renderUI({
    if (input$menu=="charts") {
      selectizeInput("chart_filters","Select State:",rbind('All','Offshore',levels(st.codes$state)))
    } else {
      selectizeInput("data_type","View Statistic:",c('sum','diff'))
    }  
  })
  
  output$filter2 <- renderUI({
      
      sliderInput("daterange", "Date range:",
                  min = 1991,
                  max = 2014,
                  value = 1)

  })
  output$filter3 <- renderUI({
    if (input$menu=="charts") {
      sliderInput("daterange3", "Date range:",
                  min = 1992,
                  max = 2014,
                  value = 1)
    } else {
    
    if (input$data_type == 'sum') {
      
      checkboxInput("texas","Exclude Texas")
      
    } else if (input$data_type == 'diff') {
      
      sliderInput("daterange3", "Date range:",
                  min = 1992,
                  max = 2014,
                  value = 1)
    
    }
    }
  })
  
  output$other_regions <- renderPlot({
    plot_data=sortdata(as.data.frame(render_data(other.codes,input$category,datasetInput(),FALSE,second_Cond())),4)
    if (input$data_type=='sum'){
      barplot(height=plot_data$sum,names.arg=plot_data$state,width=60,space=0.5,col=c('darkgreen','lightgreen'))
  } else {
      barplot(height=plot_data$sum,names.arg=plot_data$state,width=60,space=0.5,col=c('darkgreen','lightgreen'))
    }
    
  })
  
  output$chart <- renderPlot({
    if (input$chart_filters=='All') {
      plot.xts(chart_data(total.codes,input$category,timerangeInput(),'US'))  
    } else if (input$chart_filters=='Offshore') {
      plot.xts(chart_data(other.codes,input$category,timerangeInput(),'FX'))
    } else {
      plot.xts(chart_data(st.codes,input$category,timerangeInput(),input$chart_filters))
    }
  })
  
  output$total_US <- renderText({
    if (input$data_type=='sum') {
      paste("Total US Withdrawals: ",render_data(total.codes,input$category,datasetInput(),texasInput(),second_Cond())[3])
    } else{
      paste("Total US Withdrawals: ",render_data(total.codes,input$category,datasetInput(),texasInput(),second_Cond())[4])
    }
  })
  
  output$total_con_US <- renderText({
    if (input$data_type=='sum') {
      paste("Total US Consumption: ",render_data(total.con.codes,'Consumption',datasetInput(),texasInput(),second_Cond())[3])
    } else{
      paste("Total US Consumption: ",render_data(total.con.codes,'Consumption',datasetInput(),texasInput(),second_Cond())[4])
    }
  })
  
  output$chart2 <- renderPlot({
    if (input$chart_filters=='All') {
      plot.xts(chart_data(total.con.codes,'Consumption',timerangeInput(),'US'))  
    } else if (input$chart_filters=='Offshore') {
      plot.xts(chart_data(other.con.codes,'Consumption',timerangeInput(),'3F'))
    } else {
      plot.xts(chart_data(cn.codes,'Consumption',timerangeInput(),input$chart_filters))
    }
  })
  
  output$map <- renderGvis({
    
    gvisGeoChart(render_data(st.codes,input$category,datasetInput(),texasInput(),second_Cond()),"full",input$data_type,
                 options=list(region="US",displaymode="regions",
                              resolution="provinces",width="auto",
                              height=600,defaultColor="#808080"))
  })
  output$mapData <- renderGvis({
    if (input$data_type=='sum') {
      gvisColumnChart(sortdata(as.data.frame(render_data(st.codes,input$category,datasetInput(),texasInput(),second_Cond())),10),"state",input$data_type,
                    options=list(dataOpacity=0.5))
    } else {
      gvisColumnChart(sortdiffdata(as.data.frame(render_data(st.codes,input$category,datasetInput(),texasInput(),second_Cond())),5),"state",input$data_type,
                      options=list(dataOpacity=0.5))
    }
  })
  
  output$map_cons <- renderGvis({
    
    gvisGeoChart(render_data(cn.codes,"Consumption",datasetInput(),texasInput(),second_Cond()),"full",input$data_type,
                 options=list(region="US",displaymode="regions",
                              resolution="provinces",width="auto",
                              height=600,defaultColor="#808080"))
  })
  
  output$other_con_regions <- renderPlot({
    plot_data=sortdata(as.data.frame(render_data(other.con.codes,'Consumption',datasetInput(),FALSE,second_Cond())),4)
    if (input$data_type=='sum'){
      barplot(height=plot_data$sum,names.arg=plot_data$state,width=60,space=0.5,col=c('darkgreen','lightgreen'))
    } else {
      barplot(height=plot_data$sum,names.arg=plot_data$state,width=60,space=0.5,col=c('darkgreen','lightgreen'))
    }
  })
  
  output$map_consData <- renderGvis({
    if (input$data_type=='sum') {
      gvisColumnChart(sortdata(as.data.frame(render_data(cn.codes,"Consumption",datasetInput(),texasInput(),second_Cond())),10),"state",input$data_type,
                      options=list(dataOpacity=0.5))
    } else {
      gvisColumnChart(sortdiffdata(as.data.frame(render_data(cn.codes,"Consumption",datasetInput(),texasInput(),second_Cond())),5),"state",input$data_type,
                      options=list(dataOpacity=0.5))
    }
  })

})