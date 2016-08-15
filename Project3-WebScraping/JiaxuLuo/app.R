### libraries and functions
library(shiny)
library(shinydashboard)
library(googleVis)
library(data.table)
library(dplyr)
library(plotly)


Pos_bias_table_join=fread("Pos_bias_table_join.csv")
Neg_bias_table_join=fread("Neg_bias_table_join.csv")
Neutural_table_join=fread("Neutural_table_join.csv")

choice_A=read.csv("choice_of_neutral.csv",stringsAsFactors = F)
choice_of_neutral=choice_A$x
choice_B=read.csv("choice_of_Posbias.csv",stringsAsFactors = F)
choice_of_Posbias=choice_B$x
choice_C=read.csv("choice_of_Negbias.csv",stringsAsFactors = F)
choice_of_Negbias=choice_C$x

## Setting
g <- list(
        showframe = F,
        showcoastlines = T,
        projection = list(type = 'Mercator'),
        showland = T,
        landcolor = toRGB("grey83"),
        subunitcolor = toRGB("white"),
        countrycolor = toRGB("white"),
        showlakes = TRUE,
        lakecolor = toRGB("white"),
        showsubunits = T,
        showcountries = TRUE,
        resolution = 150
        
)


## Define the function 1
Fun_neutral=function(a,b){
        mm=filter(a,Neutural_website==b) %>%
                select(indicator,country,Neutural_website,countryName_G.y,CODE)
        return(mm)
}
Fun_find_country=function(a,b){
        mm=filter(a,Neutural_website==b) %>%
                select(countryName_G.y,Neutural_website)
        return(mm)
}


## Define the function 2
Fun_Positive=function(a,b){
        mm=filter(a,bias_website==b) %>%
                select(indicator,country,bias_website,countryName_G.y,CODE)
        return(mm)
}
Fun_find_country2=function(a,b){
        mm=filter(a,bias_website==b) %>%
                select(countryName_G.y,bias_website)
        return(mm)
}




Fun_neutral(Neutural_table_join,"Google.com")

## UI side
ui=shinyUI(dashboardPage(
        dashboardHeader(title = "Top Websites"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Neutral", tabName = "map", icon = icon("map")),
                        menuItem("Positive", tabName = "map1", icon = icon("map")),
                        menuItem("Negative", tabName = "map2", icon = icon("map")),
                        menuItem("ScatterPlot", tabName = "ScatterPlot", icon = icon("lightbulb-o")),
                        menuItem("Data", tabName = "data", icon = icon("database"))
                )
                
                
        ),
        
        
        
        
        dashboardBody(
                
                
                tabItems(
                        tabItem(tabName = "map",
                                fluidRow(infoBoxOutput("country"),
                                         selectInput("Neutral",
                                                     "Website",
                                                     sort(choice_of_neutral) )
                                ),
                                fluidRow(
                                        box(plotlyOutput("map",width = "1200px"),width = "auto",status ="info" )
                                )),
                        tabItem(tabName = "map1",
                                
                                fluidRow(
                                        selectInput("Positive",
                                                    "Website",
                                                    sort(choice_of_Posbias) ),
                                        box(plotlyOutput("map1"),width = "auto",status ="info")
                                )),
                        tabItem(tabName = "map2",
                                fluidRow(
                                         selectInput("Negative",
                                                     "Website",
                                                     sort(choice_of_Negbias) )
                                ),
                                fluidRow(
                                        box(plotlyOutput("map2"),width = "auto",status ="info",height="800")
                                )),
                        tabItem(tabName = "ScatterPlot",
                                fluidRow(
                                        box(plotlyOutput("ScatterPlot",width = "800px"),height = 660,width = 12,status ="info",title = h4( "Indicators of ranks in two standards") )
                                ))
                        
                        
                        
                )
        )
))


## Server Side
server=shinyServer(function(input, output){
        
        
        
        ## show map using googleVis
        output$map = renderPlotly({
                p=plot_ly(Fun_neutral(Neutural_table_join,input$Neutral), z = indicator, text = country, locations = CODE , type = 'choropleth',
                          color = indicator,  marker = list(line = l),
                          colorbar = list(tickprefix = '$', title = 'GDP Billions US$'),
                          colors=colorRampPalette(c("red" ,"yellow" ,"dark green"))(length(Fun_neutral(Neutural_table_join,input$Neutral)[,1]))
                          ) 
                layout(p,title = "Ranks of neutral website among countries",
                       geo = g)
        })
        ##
        output$map1 = renderPlotly({
                p=plot_ly(Fun_Positive(Pos_bias_table_join,input$Positive), z = indicator, text = country, locations = CODE , type = 'choropleth',
                          color = indicator, marker = list(line = l),
                          colorbar = list(tickprefix = '$', title = 'GDP Billions US$'),
                   colors=colorRampPalette(c("red" ,"yellow" ,"dark green"))(length(Fun_Positive(Pos_bias_table_join,input$Positive)[,1]))
                )
                layout(p,title = "Ranks of positive biased website among countries",
                       geo = g)
        })
        
        ##
        output$map2 = renderPlotly({
                p=plot_ly(Fun_Positive(Neg_bias_table_join,input$Negative), z = indicator, text = country, locations = CODE , type = 'choropleth',
                          color = indicator, marker = list(line = l),
                          colorbar = list(tickprefix = '$', title = 'GDP Billions US$'), 
                colors=colorRampPalette(c("red" ,"yellow" ,"dark green"))(length(Fun_Positive(Neg_bias_table_join,input$Negative)[,1]))
                )
                layout(p,title = "Ranks of negative biased website among countries",
                       geo = g)
        })
        ##
        output$ScatterPlot = renderPlotly({
                p=plot_ly(scatterplot_table, x = indicator2, y = indicator,
                             color = countryName_G, colors = "Set1", mode = "markers",text=scatterplot_table$Top_Globe)
                layout(p,geo=g)
        })
        # show statistics using infoBox
        output$country <- renderInfoBox({
                max_state <- Fun_find_country(Neutural_table_join,input$Neutral)$countryName_G.y[1]
                infoBox(max_state,icon = icon("flag"))
        })
        
})

        shinyApp(ui = ui, server = server)   
        
        
      
