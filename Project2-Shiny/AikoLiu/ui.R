shinyUI(dashboardPage(
    dashboardHeader(title = "US GDP Data App"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("US GDP Map", tabName = "map1", icon = icon("globe"),badgeLabel='Cross Sectional',badgeColor='light-blue'),
            menuItem("US GDP Plot", tabName = "plot1", icon = icon("globe"),badgeLabel='Time Series',badgeColor='blue'),
            menuItem("Bubbles", tabName = 'bubble', icon = icon('cloud'),badgeLabel='Top!',badgeColor='green'),
            menuItem("Bubble Chart", tabName = 'bubble2', icon = icon('cloud'),badgeLabel='Fun!',badgeColor='orange'),
            menuItem("2D Heatmap", tabName = 'heat', icon = icon('fire'),badgeLabel='Hot!!',badgeColor='red')
        ),
        selectizeInput("selected",
                       "Select Data to Display",
                       choice,selected="real GDP growth"),
        conditionalPanel(
        condition = "input.selected!=\"personal income per capita\" && input.selected!=\"population\" && input.selected!=\"per capita real GDP growth\" && input.selected!=\"per capita real GDP\"",  
        selectizeInput("sector", "Select Sector to Display", c('all', sectors))),
        sliderInput("slider1", "Slider input Year:", min=2000, max=2015, value=2000,animate=animationOptions(interval=2500))
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "map1",
                    fluidRow(box(htmlOutput("title1"),width=12,background='light-blue')),
                    fluidRow(infoBoxOutput("maxBox"),
                             infoBoxOutput("medBox"),
                             infoBoxOutput("minBox")),
                    #fluidRow(box(htmlOutput("map"), title='US GDP Colored Map', solidHeader = F,background='teal')), #570 on the web, 380 local
                    fluidRow(htmlOutput("map"), title='US GDP Colored Map')), #570 on the web, 380 local
                    #fluidRow(box(htmlOutput('table1'),height = 325,width=5),
                    #         box(htmlOutput('barPlot'),height= 325,width=7,background='teal'))),
                    #fluidRow(htmlOutput('table1'), htmlOutput('barPlot'))),
            tabItem(tabName = "plot1",
                            fluidRow(box(htmlOutput("title2"),width=12,background='orange')),
                            fluidRow(box(htmlOutput("warning"),width=12,background='red')),
                            fluidRow(box(htmlOutput('linePlot'), height=225,width = 12,background='teal')),
                            fluidRow(box(conditionalPanel(
                                       condition = "1 == 1",
                                       selectizeInput("state1","Select a state name",c('None',states)),height=50,width=6),background='teal'),
                                     box(conditionalPanel(
                                       condition = "input.state1!=\"None\"",  
                                       selectizeInput("state2", "Select Another state name", states),height=50,width=6),background='teal')),
                            fluidRow(box(conditionalPanel(
                                       condition = "input.state1=='None'",
                                       selectizeInput("region1","Select a US region",c('None',usRegions)),height=50,width=6),background='teal'),
                                     box(conditionalPanel(
                                       condition = "input.region1!=\"None\"&& input.state1=='None'",  
                                       selectizeInput("region2", "Select Another US region", usRegions), height=50,width=6),background='teal'))),      
            tabItem(tabName = "bubble",
                    fluidRow(box(htmlOutput("title3"),width=12,background='navy')),
                    fluidRow(box(bubblesOutput("bubble",height='550px',width = '490px'), height=550,width = 12,background='olive')),
                    fluidRow(box(sliderInput("slider2", "Slider input Top n:", min=3, max=15, value=10)))),
            tabItem(tabName = "bubble2",
                    fluidRow(box(htmlOutput("titleB"),width=12,background='navy')),
                    fluidRow(box(htmlOutput("projection"),height=80,width=4,background='green'),
                      box(selectizeInput("coordX", "X coordinate", choices=as.vector(sectorsAbbreviations),selected='IT'), height=80,width=4,background='green'),
                      box(selectizeInput("coordY", "Y coordinate", choices=as.vector(sectorsAbbreviations),selected='Mining'), height=80,width=4,background='green')),
                    fluidRow(box(htmlOutput("whichStates"),height=80,width=4,background='light-blue'),
                             box(selectizeInput("stateA", "first state", choices=states,selected='California'), height=80,width=4,background='light-blue'),
                             box(selectizeInput("stateB", "second state", choices=states,selected='New York'), height=80,width=4,background='light-blue')),
                    fluidRow(box(htmlOutput("bubble2"), height=325,width = 8,background='blue'))), #9 on web, 12 local
            tabItem(tabName = "heat",
                    fluidRow(box(htmlOutput("title4"),width=12,background='red')),
                    fluidRow(box(plotOutput("heat"),height=420,width = 12,background='navy')),
                    fluidRow(box(selectizeInput('heatX',"Heat map X variable:",heatXChoices),height=100,width=4,background='navy'),
                             box(selectizeInput('heatY',"Heat map Y variable:",heatYChoices),height=100,width=4,background='navy'),
                             box(selectizeInput('heatR',rela_abso,heatRelative),height=100,width=4,background='navy'))
            )
            
            
            
        ))
))