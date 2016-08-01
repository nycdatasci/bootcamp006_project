column(3,conditionalPanel(condition="input.tsp == 'gettingstarted'",
                          fluidRow(  uiOutput("tp.gettingstartedside")) 
                                     
),conditionalPanel(condition="input.tsp == 'facematch'",
                   fluidRow(uiOutput("tp.facematchside")
                     
               
                   
                              )
),conditionalPanel(condition="input.tsp == 'featureanalysis'",
                   fluidRow(uiOutput("tp.featureanalysisside"))
                              
),conditionalPanel(condition="input.tsp == 'imageanalysis'",
                   fluidRow(uiOutput("tp.imageanalysisside") 
                              )
),conditionalPanel(condition="input.tsp == 'findface'",
                   fluidRow(uiOutput("tp.findfaceside") 
                              )
),
conditionalPanel(condition="input.tsp == 'welcome'",
                 fluidRow("")
)
)