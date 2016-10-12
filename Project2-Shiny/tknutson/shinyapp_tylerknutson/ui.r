


###START THE APP

ui <- 
  dashboardPage(
    skin="purple",
    
    dashboardHeader(
      title="Vizualizing California Wine",
      titleWidth = 700
    ),
    
    dashboardSidebar(
      
      sliderInput(
        "yearSlider", label = "Slider input:", min = 2010, max = 2015, value = 2010, sep = ""  
      ),
      selectInput(
        "appellationSelect", label = "Select Appellation:", choices = c('All',as.character(unique(api_all$appellation_name)))),
      
      selectInput(
        "wineType", label = "Select Wine Type:", choices = c("All Types", "Red Wines", "White Wines"), multiple = FALSE, selected = "All Types" 
      ),
      uiOutput(
        "varietal_name"),
      
      radioButtons(
        "scaleType", label = "Select Scale for Heatmap:", choices = c("Price", "Value Score", "Volume")
      )#,
      #radioButtons(
      #  "topLinevalue", label = "select Value to Plot on Top Chart:", choices = c("Tons Crushed", "Tons Purchased", "Average Brix", "Average Price per Ton")
      #)
    ),
    dashboardBody(
       
        fluidRow(
          valueBoxOutput("PriceBox", width = 3),
          valueBoxOutput("ScoreBox", width = 3),
          valueBoxOutput("ValueBox", width = 3),
          valueBoxOutput("TotalBox", width = 3)
          ),
        
        tabsetPanel(
          
         tabPanel("Wine.com Catalog",
            fluidRow(
            
          box(leafletOutput("wineMap", width = 775, height = 680)),
              
          box(plotOutput("apiScatter", hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"), width = "100%"),
              uiOutput("hover_info"),
              height = 530),
          box(
            
            sliderInput(
            "priceSlider", label = "Select Maximum Price on Graph:", min = 0, max = 2000, value = 2000, sep = ""), height = 150)
          ))
         ,
          
          tabPanel("California Agricultural Data",
            fluidRow(
             # column(
                box(htmlOutput("line1"), width = 12),
                #box(htmlOutput("line2"), width = 6),
                box(htmlOutput("line3"), width = 6),
                box(htmlOutput("line4"), width = 6)
          ))
        )
    )
  )

