
library(devtools)
library(shiny)
library(DT)
library(bubbles)
library(googleVis)
library(treemap)
library(d3treeR)


## Server Side
shinyServer(function(input, output){
        colfunc=colorRampPalette(c("grey","pink"))
        
        ## show map using googleVis
        output$map = renderGvis({
                gvisGeoChart(Fuc_Map(Fortune,input$Year,input$Industry), "Country", "Count",
                             options=list(width="auto", height="490px"))
        })
        
        ## show treemap using "treemap","d3treeR" packages
        output$treemap = renderD3tree2({d3tree2( Fuc_Treemap(Fortune,input$Year,input$Industry),rootname = "Market value by Industry" )
                
        })
        
        ## show bubbles using "bubbles" package
        output$bubbles=renderBubbles(
                bubbles(value=sqrt(Fuc_Bubble(Fortune,input$Year,input$Industry)[,"Market.value..m"]),
                        label=Fuc_Bubble(Fortune,input$Year,input$Industry)[,"Company"],
                        textColor = "#00008B",
                        color=colorRampPalette(c("light green", "yellow", "orange", "red"))(length(Fuc_Bubble(Fortune,input$Year,input$Industry)[,"Company"])))
        )
        
        ## show histogram using googleVis
        output$hist=renderGvis({
                gvisBarChart(Fuc_Hist(Fortune,input$Year,input$Industry),xvar = "Company",yvar="P.e.ratio",options = list(width="400px",height="550px"))
                #(state_stat[,input$selected, drop=FALSE])
                
        })
        
        ## show data using DataTable
        output$table = DT::renderDataTable({
                datatable(Fuc_Bubble(Fortune,input$Year,input$Industry)[,c(2,3,4,5,6,7,9,13,16),]
                          , rownames=FALSE) %>% 
                        # Highlight selected column using formatStyle
                        formatStyle(input$selected, background="skyblue", fontWeight='bold')
                
        })
        
        
        
        
})