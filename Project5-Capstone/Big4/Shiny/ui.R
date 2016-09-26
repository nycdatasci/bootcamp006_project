# This is the user-interface definition of a Shiny web application.
require(shinyBS)
library(shinyjs)

navbarPage(title="NYC Taxi Pickups Prediction",
           id = 'main',
           theme = shinytheme("cerulean"),
           # inverse=T,
           collapsible = T,
           tabPanel(title = icon('globe'),
                    div(class="outer",
                        tags$head(
                          includeCSS("styles.css")
                        ),
                        leafletOutput('map',width="100%",height='100%'),
                        absolutePanel(top = 10,
                                      left = 50,
                                      draggable = F,
                                      width=140,
                                      height='auto',
                                      dateInput('slt_date',label=NULL,
                                                value = NULL,min = '2016-09-22',
                                                max='2016-09-28'),
                                      uiOutput('plot_type'),
                                      uiOutput('hour_output'),
                                      bsTooltip('slt_date','Date of predictions')
                        
                        ),
                        absolutePanel(top = 10,
                                      left = 220,
                                      draggable = F,
                                      width='auto',
                                      height='auto',
                                      actionButton('btn_clr',label = icon('refresh'))
                        ),
                        absolutePanel(bottom = 20,
                                      right = 10,
                                      draggable = F,
                                      width='auto',
                                      height='auto',
                                      a(icon('github fa-2x'),
                                        href='https://github.com/nycdatasci/bootcamp006_project/tree/master/Project5-Capstone/Big4',
                                        target='_blank')
                        ),
                        uiOutput('plot_UI')
                    )
           ),
           tabPanel(title = icon('database'),
                    dataTableOutput('table')
           )
           
           
)