### libraries and functions
library(shiny)
library(DT)
library(bubbles)
library(shinydashboard)
library(googleVis)
library(treemap)
library(d3treeR)

Fortune=read.csv("Fortune.csv",stringsAsFactors = F)
choice_of_industry=c(unique(Fortune$Sector))
load("Tree.Rdata")

Fuc_Map=function (a,b,c){
        library(dplyr)
        mm=filter(Fortune,!is.na(Market.value..m))  %>%
                filter(Year==b) %>%
                filter(Sector==c) %>%
                group_by(Country) %>%
                summarise(Count=n())
        return(mm)
}

Fuc_Bubble=function (a,b,c){
        library(dplyr)
        mm=filter(Fortune,!is.na(Market.value..m))  %>%
                filter(Year==b) %>%
                filter(Sector==c) 
        return(mm)
}


Fuc_Hist=function(a,b,c){
        library(dplyr)
        Fortune[,"P.e.ratio"]=as.numeric(Fortune[,"P.e.ratio"])
        mm=filter(Fortune,!is.na(P.e.ratio)) %>%
                filter(Year==b) %>%
                filter(Sector==c) %>%
                select(Company,P.e.ratio) %>%
                arrange(desc(P.e.ratio)) %>%
                head(8)
        return(mm)
}
## UI side
ui=shinyUI(dashboardPage(
        dashboardHeader(title = "Fortune 500 dashboard"),
        dashboardSidebar(
                         sidebarMenu(
                                 menuItem("Distribution", tabName = "map", icon = icon("map")),
                                 menuItem("Composition",tabName = "treemap",icon=icon("tree")),
                                 menuItem("Comparison", tabName = "bubbles", icon = icon("circle")),
                                 menuItem("Data", tabName = "data", icon = icon("database"))
                         ),
                         
                         sliderInput("Year","Year of Interest",min=2006,max=2015,value=2006),
                         br(),
                         selectInput("Industry",
                                     "Industry of Interest",
                                     sort(choice_of_industry) )
        ),
        
        
        
        
        dashboardBody(
                tabItems(
                        tabItem(tabName = "map",
                                
                                fluidRow(
                                        box(htmlOutput("map"),width = "auto",status ="info",title = h4( "Number of Fortune 500 by industry") )
                                )),
                        tabItem(tabName = "treemap",
                                
                                fluidRow(
                                        box(d3tree2Output("treemap",height = "600px"),width = "auto",height="600")
                                )),
                        
                        tabItem(tabName = "bubbles",
                                
                                fluidRow(
                                        box(bubblesOutput("bubbles",width="600px"),height = 660,width = 7,title =h4( "Market.value(m) of companies")),
                                        box(htmlOutput("hist"),height = 660,width = 5,title=h4("P.e.ratio of companies"))
                                        
                                )),
                        
                        tabItem(tabName = "data",
                                fluidRow(box(DT::dataTableOutput("table"), width = 12)))
                )
        )
))


## Server Side
server=shinyServer(function(input, output){
        colfunc=colorRampPalette(c("grey","pink"))
        
        
        ## show map using googleVis
        output$map = renderGvis({
                gvisGeoChart(Fuc_Map(Fortune,input$Year,input$Industry), "Country", "Count",
                             options=list(width="auto", height="490px",colors="['#90EE90' ,'#FFA500', '#FF0000']"))
        })
        
        ## show treemap using "treemap","d3treeR" packages
        output$treemap = renderD3tree2({get(paste("Tree",input$Year,sep="")) 
                
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

shinyApp(ui = ui, server = server)


