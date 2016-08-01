shinyUI({
  navbarPage('Los Angeles Crimes 2011~2015',
             theme = 'bootstrap.css',
             ########## WHERE #########
             tabPanel('Crimes WHERE',
                      fluidRow(
                        column(width = 3,
                               titlePanel('Where Crimes Are'),
                               #sidebarLayout(position = 'left',
                               #fluid = TRUE,
                               #sidebarPanel(width = 2, 
                               checkboxGroupInput('CMyear', 
                                                  label = h5('Select Years'),
                                                  choices = c('2011' = 2011,
                                                              '2012' = 2012,
                                                              '2013' = 2013,
                                                              '2014' = 2014,
                                                              '2015' = 2015), 
                                                  selected = c(2011, 2012, 2013, 2014, 2015)
                               ),
                               
                               selectInput('CMarea', 
                                           label = h5('Select the Area'), 
                                           choices = c('All areas' =  1, 
                                                       crimedf2$AREA.NAME),
                                           selected = 1
                               )
                        ),
                        
                        column(width = 9,
                               #mainPanel(
                               tabsetPanel(
                                 tabPanel("Crimes Map", # leaflet map
                                          leafletOutput("mymap",
                                                        width = 'auto',
                                                        height = '700')
                                 ),
                                 tabPanel("Crimes Area", # bar chart and line charts for areas
                                        #  fluidRow(
                                            column(width = 6,
                                            htmlOutput('crm_area') # width, height parameters are useless here
                                            ),
                                            column(width = 6,
                                            htmlOutput('crm_tot_chg'),
                                            img(src='LAPD_Area.jpg', position = 'right', height = 450, width = 400)
                                            )
                                        #  )
                                 )
                                 #)
                                 #)
                                 # )
                               )
                        ) 
                      )
                      
             ),
             
             
             ########## WHEN ##########
             tabPanel('Crimes WHEN',
                      tabsetPanel(
                        tabPanel('Crimes Daily',
                                 htmlOutput('calendar')),
                        tabPanel('Crimes Week',
                                 htmlOutput('crm_days_status')
                        ))
             ),
             
             ########## WHO ##########
             tabPanel('Crimes WHO',
                      htmlOutput('crm_status')
             ),
             ########## WHY ##########
             #tabPanel('Crimes WHY'
             #),
             tabPanel('Data',
                      dataTableOutput('crm_table')
             ),
             tabPanel('Reference',
                      h4('Data Source'),
                      p('Datasets in this project are provided by ', a('Los Angeles Open Data', href = 'https://data.lacity.org/browse?category=A+Safe+City&limitTo=datasets&utf8=✓'), '.'),
                      p('This Shiny application focuses on 21 areas encompassing 467 square miles oversaw by LAPD. You can find more information about the LAPD ', a('here', href = 'http://www.lapdonline.org/inside_the_lapd/content_basic_view/1063', '.')),
                      hr()
             )
  )
}
)