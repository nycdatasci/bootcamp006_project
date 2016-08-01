library(shiny)
library(ggplot2)
library(leaflet)
library(shinythemes)
library(DT)

var = list(
  "Brooklyn" = bor[2],
  "Downtown" = bor[3],
  "Financial District" = bor[4], 
  "Midtown" = bor[5],
  "Upper East Side" = bor[6],
  "Upper Manhattan" = bor[7],
  "Upper West Side" = bor[8]
)

var2 = list('Bath Beach'=reg[2],
                      'Battery Park'=reg[3],
                      'Bay Ridge'=reg[4],
                      'Bedford-Stuyv'=reg[5],
                      'Beekman/Sutto' = reg[6],
                      "Bensonhurst" = reg[7],
                      "Bergen Beach"=reg[8],
                      "Boerum Hill"=reg[9],
                      "Borough Park"=reg[10],
                      "Brighton Beac..."=reg[11],
                      "Broadway Corr..."=reg[12],
                      "Brooklyn Heig..."=reg[13],
                      "Bushwick"=reg[14],
                      "Canarsie"=reg[15],
                      "Carnegie Hill"=reg[16],
                      "Carroll Garde..."=reg[17],
                      "Central Park ..."=reg[18],
                      "Chelsea"=reg[19],
                      "Clinton Hill"=reg[20],
                      "Cobble Hill"=reg[21],
                      "Coney Island"=reg[22],
                      "Crown Heights"=reg[23],
                      "Downtown Broo..."=reg[24],
                      "DUMBO"=reg[25],
                      "East Harlem"=reg[26],
                      "East New York"=reg[27],
                      "East Village"=reg[28],
                      "Financial Dis..."=reg[29],
                      "Flatbush"=reg[30],
                      "Flatiron/Unio..."=reg[31],
                      "Fort Greene"=reg[32],
                      "Fort Hamilton"=reg[33],
                      "Gowanus"=reg[34],
                      "Gramercy Park"=reg[35],
                      "Gravesend"=reg[36],
                      "Greenpoint"=reg[37],
                      "Greenwich Vil..."=reg[38],
                      "Greenwich Vil."=reg[39],
                      "Hamilton Heig..."=reg[40],
                      "Harlem"=reg[41],
                      "Homecrest"=reg[42],
                      "Inwood"=reg[43],
                      "Kensington"=reg[44],
                      "Lenox Hill"=reg[45],
                      "Lincoln Cente..."=reg[46],
                      "Lower East Si..."=reg[47],
                      "Madison"=reg[48],
                      "Midtown East"=reg[49],
                      "Midtown West"=reg[50],
                      "Midwood"=reg[51],
                      "Morningside H..."=reg[52],
                      "Murray Hill"=reg[53],
                      "New Lots"=reg[54],
                      "NOHO"=reg[55],
                      "NoLiTa/Little..."=reg[56],
                      "Park Slope"=reg[57],
                      "Park/Fifth Av..."=reg[58],
                      "Prospect Heig..."=reg[59],
                      "Prospect Leff..."=reg[60],
                      "Red Hook"=reg[61],
                      "Riverside Dr...."=reg[62],
                      "Roosevelt Isl..."=reg[63],
                      "Sheepshead Ba..."=reg[64],
                      "SOHO"=reg[65],
                      "South Slope -..."=reg[66],
                      "Sunset Park"=reg[67],
                      "Tribeca"=reg[68],
                      "Turtle Bay/Un..."=reg[69],
                      "Washington He..."=reg[70],
                      "West Village"=reg[71],
                      "Williamsburg"=reg[72],
                      "Windsor Terra..."=reg[73],
                      "Yorkville"=reg[74])

shinyUI(
  navbarPage('Condo Sales History', 
                   theme = shinytheme("flatly"),
             id="nav",
  tabPanel('Interactive map',

  div(class="outer",

      tags$head(
        
         #Include our custom CSS
      includeCSS("styles.css"),
      includeScript("gomap.js")
      )),
    leafletOutput("map",height = "840",width = "1800"),
   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                sliderInput('range',label=h3('Price range:'),
                                                      min=0, max = 50000000,value=0),

                checkboxGroupInput('area', label = h3('Area'),var,selected = NULL),

                uiOutput("neighbourhood"),
                selectInput('select',label=h3('Choose the number of bedrooms'),
                                                      choices = list('Studio'= bed[10], '1 bd'=bed[3],
                                                                     '2 bd'= bed[4],'3 bd'=bed[5],
                                                                     '4 bd'=bed[6],'5 bd'=bed[7],
                                                                     '6+ bd'=bed[8],'Loft bd'=bed[9]),selected=NULL),
                                          selectInput('select2',label=h3('Choose the number of bathrooms'),
                                                        choices = list('1 ba'= bath[3], '1.5 ba'=bath[4],
                                                                       '2 ba'= bath[5],'2.5 ba'=bath[6],
                                                                       '3 ba'=bath[7], '3.5 ba'=bath[8],
                                                                       '4 ba'= bath[9], '4.5 ba'=bath[10],
                                                                       '5 ba'=bath[11], '5.5 ba'=bath[12],
                                                                      '6+ ba'=bath[13]),selected=NULL))),
  # sidebarLayout(
  #   sidebarPanel(
  #               sliderInput('range',label=h3('Price range:'),
  #                           min=0, max = 50000000,value=0),
  #               
  #               checkboxGroupInput('area', label = h3('Area'),var,selected = NULL),
  #               
  #               uiOutput("neighbourhood"),
  #                   # conditionnalPanel(condition="input$area=='Midtown'",
  #                   #                    selectInput('checkGroup',label=h3('Neighborhood'),
  #                   #                                          choices = c('Beekman/Sutto...','Midtown East','Midtown West','Murray Hill','Turtle Bay/Un...'),selected=NULL)),
  #                 # 
  #                 #   conditionalPanel(condition="input$area=='Brooklyn'",
  #                 #                    selectInput('checkGroup',label=h3('Neighborhood'),
  #                 #                                choices= list('Bath Beach','Bay Ridge','Bedford-Stuyv...','Bensonhurst','Bergen Beach','Boerum Hill','Borough Park','Brighton Beac...','Brooklyn Heig...',
  #                 #                                             'Bushwick','Canarsie', 'Carroll Garde...','Clinton Hill','Cobble Hill','Coney Island',
  #                 #                                             'Crown Heights','Downtown Broo...','DUMBO','East New York','Flatbush','Fort Greene',
  #                 #                                             'Fort Hamilton','Gowanus','Gravesend','Greenpoint','Homecrest','Kensington','Madison',
  #                 #                                             'Midwood','New Lots','Park Slope','Prospect Heig...','Prospect Leff...','Red Hook','Sheepshead Ba...',
  #                 #                                             'South Slope -...','Sunset Park','Williamsburg','Windsor Terra...'),selected=NULL)
  #                 #     ),
  #                 # 
  #                 #    conditionalPanel(condition="input$area=='Downtown'",
  #                 #         selectInput('checkGroup',label=h3('Neighborhood'),
  #                 #                     choices = list('Chelsea','East Village','Flatiron/Unio...','Gramercy Park','Greenwich Vil...','Greenwich Vil.',
  #                 #                                    'Lower East Si...','NOHO','NoLiTa/Little...','SOHO','Tribeca','West Village'),selected=NULL)),
  #                 # 
  #                 #    conditionalPanel(condition="input$area=='Financial District'",
  #                 #         selectInput('checkGroup',label=h3('Neighborhood'),
  #                 #                     choices = list('Battery Park ...','Financial Dis...'),selected=NULL)
  #                 #   ),
  #                 # 
  #                 #  conditionalPanel(condition="input$area=='Upper East Side'",
  #                 #         selectInput('checkGroup',label=h3('Neighborhood'),
  #                 #                     choices = list('Carnegie Hill','Park/Fifth Av...','Roosevelt Isl...','Yorkville'),selected=NULL)),
  #                 #   conditionalPanel(condition="input$area=='Upper Manhattan'",
  #                 #       selectInput('checkGroup',label=h3('Neighborhood'),
  #                 #                   choices = list('East Harlem','Hamilton Heig...','Harlem','Inwood','Washington He...'),selected=NULL)),
  #                 # 
  #                 # conditionalPanel(condition="input$area=='Upper West Side'",
  #                 #       selectInput('checkGroup',label=h3('Neighborhood'),
  #                 #                   choices = list('Broadway Corr...','Central Park ...','Lenox Hill','Lincoln Cente...','Morningside H...','Riverside Dr....'),selected=NULL)),
  #                 # 
  #               
  #               selectInput('select',label=h3('Choose the number of bedrooms'),
  #                           choices = list('Studio'= bed[10], '1 bd'=bed[3],
  #                                          '2 bd'= bed[4],'3 bd'=bed[5],
  #                                          '4 bd'=bed[6],'5 bd'=bed[7],
  #                                          '6+ bd'=bed[8],'Loft bd'=bed[9]),selected=NULL),
  #               selectInput('select2',label=h3('Choose the number of bathrooms'),
  #                             choices = list('1 ba'= bath[3], '1.5 ba'=bath[4],
  #                                            '2 ba'= bath[5],'2.5 ba'=bath[6],
  #                                            '3 ba'=bath[7], '3.5 ba'=bath[8],
  #                                            '4 ba'= bath[9], '4.5 ba'=bath[10],
  #                                            '5 ba'=bath[11], '5.5 ba'=bath[12],
  #                                           '6+ ba'=bath[13]),selected=NULL)
  #           ),
  #     mainPanel(
  #       leafletOutput("map",width = "1200",height = "900"))
  #   )) ,
    tabPanel('Explore Data',
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput('District',
                                    label = h3('Area'),
                                    choices = list(
                                      "Brooklyn" = bor[2],
                                      "Downtown" = bor[3],
                                      "Financial District" = bor[4],
                                      "Midtown" = bor[5],
                                      "Upper East Side" = bor[6],
                                      "Upper Manhattan" = bor[7],
                                      "Upper West Side" = bor[8]
                                    ),selected = NULL)

    #              #                "Date range",
    #              #                start = "2016-01-01",
    #              #                end = "2016-07-18"))
    #
    #
               ),
               mainPanel(
                 dygraphOutput("dygraph")

               )
             )
    ),
  tabPanel('Reference',
    fluidPage(
      column(8, dataTableOutput('tbl'))
    )
    
  )
))

