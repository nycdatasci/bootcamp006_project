suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(leaflet))

shinyUI(dashboardPage(
    skin = 'purple',
    dashboardHeader(title = "Automated Driving"), # Create header top left


# Create sidebar ----------------------------------------------------------

    dashboardSidebar(
        sidebarMenu( # Create menu on left side
            menuItem(
                "How does this affect me?",
                tabName = "intro",
                icon = icon("comment")
            ),
            menuItem(
                "Number of Accidents",
                tabName = "us_map",
                icon = icon("map")
            ),
            menuItem(
                "Accident Locations",
                tabName = "acc_loc",
                icon = icon("map")
            ),
            menuItem(
                "Transportation Jobs",
                tabName = "trans",
                icon = icon("map")
            ),
            menuItem("Data", tabName = "data", icon = icon("database"))
        )
    ),



# Create tab bodies -------------------------------------------------------

    dashboardBody(
        tabItems(
        tabItem(tabName = "intro", # Create intro tab
                div(id = 'intro',
                    tags$head(# Include custom CSS
                        includeCSS("02_scripts/styles_drs.css")),
                    fluidRow(column(
                        4,
                        h1('Automated Driving'),
                        h4('Americans drive millions of miles every year.'),
                        h4(
                            'Motor Vehicle accidents are consistently among the top ten causes of death in the United States.'
                        ),
                        h4(
                            'Automated driving, either cars helping humans or independently driving, promises to reduce the number of fatal car accidents each year.'
                        ),
                        h4(
                            'The downside to this new technology is that it poses a serious problem for the workers in the transportation industry.'
                        ),
                        h4(
                            'The U.S. Department of Transportation estimates 4 million people work in the transportation industry. Could their jobs be at risk?'
                        ),
                        h4(
                            'This app will help you answer how automated driving might impact you by exploring data about traffic accidents and transportation employment in your area.'
                        ),
                        p(
                            'The data is from 2014 and comes from the NHTSA and BLS. It can be found at ftp://ftp.nhtsa.dot.gov/FARS/ and http://www.bls.gov/oes/special.requests/oes_research_2014_sec_48-49.xlsx'
                        )
                    )))), 
            
        tabItem(tabName = "us_map", # Create tab with accident maps

                fluidRow(column(
                    6,
                    div(
                        align = 'center',
                        h4('Number of Fatal Accidents'),
                        box(htmlOutput("us_map"), width = 12)
                    ),
                    box(
                        p(
                            'This map shows the absolute number of fatal car accidents per state for the year 2014. Texas, California, and Florida have large number of fatal accidents, but they also have higher populations than most states.'
                        ),
                        width = 12
                    )
                ),

                column(
                    6,
                    div(
                        align = 'center',
                        h4('Number of Fatal Accidents Per 100,000 People'),
                        box(htmlOutput("us_map_bypop"), width = 12)
                    ),
                    box(
                        p(
                            'This map shows the number of fatal car accidents per 100,000 people per state for the year 2014. Here one can see states such as California fare better when considering the number of people in the state. Wyoming has one of the lowest rates of overall accidents, but you are most likely to die in a car accident there based on the per capita rate.'
                        ),
                        width = 12
                    )
                ))), 
        
        tabItem(tabName = "acc_loc", # Create tab with interactive map
                div(
                    class = "outer",
                    tags$head(# Include custom CSS
                        includeCSS("02_scripts/styles_drs.css")),
                    leafletOutput("map_lf", width = "100%", height = "100%"),
                    absolutePanel(
                        id = "hover_box",
                        class = "panel panel-default",
                        fixed = FALSE,
                        draggable = TRUE,
                        top = 20,
                        left = "auto",
                        right = 20,
                        bottom = "auto",
                        width = 330,
                        height = "auto",
                        div(align = 'center', h3('Visible Accidents')),
                        plotOutput("bar_hr", height = 180),
                        plotOutput("bar_mn", height = 180),
                        plotOutput("bar_dy", height = 180),
                        textOutput("tot")
                    )
                )), 

        tabItem(tabName = "trans", # Create tab with jobs maps
                fluidRow(column(
                    6,
                    div(
                        align = 'center',
                        h4('Number of Jobs in the Transportation Industry'),
                        box(htmlOutput("trans_map"), width = 12)
                    ),
                    box(
                        p(
                            'This map shows the absolute number of jobs in the transportation industry per state for the year 2014. Texas, California, and New York have large number of transportation industry jobs, but they also have higher populations than most states.'
                        ),
                        width = 12
                    )
                ),
                column(
                    6,
                    div(
                        align = 'center',
                        h4('Transportation Jobs per 100 People'),
                        box(htmlOutput("trans_map_bypop"), width = 12)
                    ),
                    box(
                        p(
                            'This map shows the number of transportation jobs per 100 people per state for the year 2014. This number was calculated by dividing the number of transportation jobs by the population of each state and multiplying by 100. Here one can see states such as North Dakota and Nebraska have more jobs per 100 people than states with large overall numbers.'
                        ),
                        width = 12
                    )
                ))), 
        
        tabItem(tabName = "data",
            fluidRow(box(DT::dataTableOutput("table"), width = 12))
        )
    ))
)
)
