column(8,conditionalPanel(condition="input.tsp == 'gettingstarted'",
                         fluidRow(  uiOutput("tp.gettingstarted") )#,plotOutput("gettingstarted", width="100%", height="auto"
                                                                               
),conditionalPanel(condition="input.tsp == 'facematch'",
                   fluidRow(  uiOutput("tp.facematch") )
                   #,plotOutput("facematch", width="100%", height="auto"
                               
),conditionalPanel(condition="input.tsp == 'featureanalysis'",
                   fluidRow(  uiOutput("tp.featureanalysis") )
                   #,plotOutput("featureanalysis", width="100%", height="auto")
),conditionalPanel(condition="input.tsp == 'imageanalysis'",
                   fluidRow(  uiOutput("tp.imageanalysis") )
                   #, plotOutput("imageanalysis", width="100%", height="auto")
),conditionalPanel(condition="input.tsp == 'findface'",
                   fluidRow(  uiOutput("tp.findface") )
                   #,plotOutput("findface", width="100%", height="auto")
),
conditionalPanel(condition="input.tsp == 'welcome'",
                 fluidRow(column(11,tabPanelAbout),column(1,"") ))
)

