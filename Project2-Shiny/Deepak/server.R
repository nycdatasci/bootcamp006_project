library(DT)
library(shiny)
library(googleVis)

shinyServer(function(input, output){
    # show map using googleVis
    output$map <- renderGvis({
        gvisGeoChart(state_stat, "state.name", input$selected1,
                     options=list(region="US", displayMode="regions", 
                                  resolution="provinces",
                                  width="auto", height="auto")
                     )
    })
    
    # show histogram using googleVis
    output$hist1 <- renderGvis({
        gvisHistogram(state_stat[,input$selected1, drop=FALSE],
                      option=list(
                        #title=input$selected1 ,
                        legend="{ position: 'top',maxLines: 1 }",
                        #width=800,
                        height=500
                        )
                      )
    })
    
    output$column <- renderGvis({
      M = input$slider1
      gvisColumnChart(head(PT,M),
                   option=list(
                     #width=600,
                     height=600,
                     title ='H-1B VISA applications processing time',
                     hAxis= list(title = 'Processing Time (days)')
                     #vAxis= list(title = 'Number of Applications'),
                     
                   )
                   
      )
    })
    
    output$dygraph <- renderDygraph({
      dygraph(ts_data, main = "H-1B VISA - Submitted Applications and Processed Applications with time",ylab = "Number of Applications",height = 600) %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)
                      
                      
      
    })
    
    
    
    
    output$pie <- renderGvis({
      gvisPieChart(data.frame("state" =state_stat["state.name"],state_stat[,input$selected1, drop=FALSE]),
                    option=list(
                      title=input$selected1,
                      is3D= TRUE,
                      #slices="{4: {offset: 0.2}, 0: {offset: 0.3}}",
                      #legend="{ position: 'top',maxLines: 1 }",
                      #width=1000,
                      height=600
                    )
      )
    })
    
    output$pie1 <- renderGvis({
      N = input$slider2
      if (input$selected2 == "STATES") { 
        df = head(certified_state,N)
      } else if (input$selected2 == "EMPLOYERS"){
        df = head(certified_employers,N)
      } else if (input$selected2 == "JOB TITLES"){ 
        df = head(certified_job_title,N)
      } else {
        df = head(certified_occ,N)
      }
      
      
      gvisBarChart(df,
                   option=list(
                     #width=800,
                     height=550,
                     #hAxis= list(title = 'Number of Applications Approved  for H-1B VISA'),
                     title = paste(paste('TOP ' , input$slider2 ),input$selected2)
                   )
                   
      )
    })
    
    output$pie2 <- renderGvis({
      N = input$slider2
      if (input$selected2 == "STATES") { 
        df = head(avg_salary_state,N)
      } else if (input$selected2 == "EMPLOYERS"){
        df = head(avg_salary_employers,N)
      } else if (input$selected2 == "JOB TITLES"){ 
        df = head(avg_salary_job_title,N)
      } else {
        df = head(avg_salary_occ,N)
      }
      
      gvisBarChart(df,
                   option=list(
                     #width=1000,
                     height=550,
                     #hAxis= list(title = 'Average Salary ($)')
                     title = paste(paste('TOP ' , input$slider2 ),input$selected2)
                   )
                   
      )
    })
    
    

    output$bar <- renderGvis({
      df <- data.frame(state_stat["state.name"],state_stat[input$selected1])
      gvisBarChart(df,
                   option=list(
                    width=1000,
                    height=600
                    )
                              
                  )
    })
    
    
    # show data using DataTable
    output$table <- DT::renderDataTable({
        datatable(state_stat, rownames=FALSE) %>% 
            formatStyle(input$selected1, background="skyblue", fontWeight='bold')
    })
    
    # show statistics using infoBox
    output$maxBox <- renderInfoBox({
        max_value <- max(state_stat[,input$selected1])
        max_state <- 
            state_stat$state.name[state_stat[,input$selected1] == max_value]
        infoBox(max_state, max_value, icon = icon("hand-o-up"))
    })
    output$minBox <- renderInfoBox({
        min_value <- min(state_stat[,input$selected1])
        min_state <- 
            state_stat$state.name[state_stat[,input$selected1] == min_value]
        infoBox(min_state, min_value, icon = icon("hand-o-down"))
    })
    output$avgBox <- renderInfoBox(
        infoBox(paste("NATIONAL AVERAGE of ", input$selected1),
                as.integer(lapply(state_stat[, input$selected1],mean)), 
                icon = icon("calculator")))
})