ui = fluidPage(
  theme = "slate.css",
  titlePanel("European Soccer Leagues"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sea",label="Season",
                  choices=unique(df_sea$season),
                  selected="2015/2016"),
      selectInput("lea",label="League",
                  choices=unique(df_sea$country),
                  selected="England"),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Soccer League Map",leafletOutput("myMap",height=600)),
        tabPanel("Trend by Season",plotOutput("myLinechart")),
        tabPanel("League by Season",plotlyOutput("myTeamchart")),
        tabPanel("Lookup Table",dataTableOutput("myTable"))
      ),
      width=10
    )
  )
)