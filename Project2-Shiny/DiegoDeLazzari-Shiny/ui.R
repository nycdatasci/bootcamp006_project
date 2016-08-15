
shinyUI(
  # fluidPage(
  # titlePanel('Tracking migration paths from the Middle East'),
  dashboardPage(
    dashboardHeader(title = "Navigation"),
    dashboardSidebar(
        #sidebarUserPanel("Diego De Lazzari",
        #                image = "photo.jpg"),
        sidebarMenu(
            menuItem("About", tabName = "about", icon = icon("info")),
            menuItem("Migration routes", tabName = "destination", icon = icon("map")),
            menuItem("Analytics", tabName = "stats", icon = icon("bar-chart")),
            menuItem("Data", tabName = "data", icon = icon("database"))
                  )
                  ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "data",
                    fluidRow(
                      column(width = 9,
                             box(width = NULL, dataTableOutput("table")
                            )
                            ),
                      column(width = 3,
                             box(width = NULL, status = "primary", solidHeader = TRUE, title = "Parameters",
                                 helpText("Display of the data used in the project"),
                                 selectInput("whichData", label = "Step 1: Chose Dataset:",
                                             choices = availDatasets, selected = availDatasets[1]
                                             ),
                                 selectInput("selected", label = "Step 2: Select Items to Display:", choices = col[-1],
                                             multiple = T, selected = col[1:4])
                                 )
                             )
                             )
                    ),
            tabItem(tabName = "about",
                    fluidRow(
                      box(title = "Tracking migration paths through Eastern and Southern Europe", status = "primary", solidHeader = TRUE, collapsible = F, width = 12,
                         br(),
                         tags$p('The project attempts to visualize the migration paths followed by over a million migrants in the last 
                              18 months, by means of an interactive map developed in Shiny. While offering a dynamic picture of the migration flow through Eastern and Southern Europe, 
                              the app aims at analyzing the composition of such flow in terms of country of origin and gender.'),
                           br(),
                           tags$p("The dataset is based on the estimated daily arrivals recorded by the hosting countries and gathered by the UN Refugee Agency (UNHCR). For more information,
                              visit the blog or the UNHCR website:"),
                         tags$hr(),
                         tags$a(href = "http://blog.nycdatascience.com/student-works/r-shiny/shiny-tracking-migration-paths-eastern-europe/", " Blog post: Tracking migration paths through Eastern Europe", style = "font-size: 18px;"),
                         tags$br(),
                         tags$br(),
                         tags$a(href = "http://data.unhcr.org/mediterranean/regional.php", "UNHCR: About refugee emergency on the Mediterranean", style = "font-size: 18px;"),
                         tags$br(),
                         tags$a(href = "http://data.unhcr.org/syrianrefugees/regional.php", "UNHCR: Refugees in Syria", style = "font-size: 18px;")
                        ),
                      box(title = "About me", status = "primary", solidHeader = TRUE, collapsible = F, width = 12,
                          column(
                              width = 2,
                              br(),
                              br(),
                              tags$img(
                                src = 'DiegoDeLazzari.jpg',
                                width = "100px", height = "100px"),
                              br(),
                              br(),
                              br(),
                              tags$a(href = "https://www.linkedin.com/in/diegodelazzari", icon("linkedin", "fa-2x")),
                              tags$a(href = "https://github.com/nycdatasci/bootcamp006_project/tree/master/Project2-Shiny/DiegoDeLazzari-Shiny", icon("github", "fa-2x"), style = "margin-left: 20px;")
                              ),
                            column(
                              width = 10,
                              h4("Diego De Lazzari"),
                              tags$p("Researcher, developer and data scientist(?). Diego De Lazzari is an applied physicist with a rather
                                      diverse background. He spent 8 years in applied research, developing computational 
                                      models in the field of Plasma Physics (Nuclear Fusion) and Geophysics. As a Research 
                                      Geophysicist at Shell Global Solutions, He developed data-driven applications
                                      to predict and extract small, usually unwanted features in large seismic datasets. 
                                      Diego holds a Ph.D. in Applied physics from Eindhoven University of Technology and is 
                                      interested in energy industry, energy scenario modeling and global trends for business 
                                      and society."),
                              
                              tags$hr(),
                              h4("This project was completed for the NYC Data Science Academy. More info at: "),
                              tags$a(href = "http://nycdatascience.com/", "NYC Data Science Academy", style = "font-size: 18px;")
                              )
                          )
                           )
                    ),
            tabItem(tabName = "destination",
                    fluidRow(
                      box(width = 12,  status = "primary", solidHeader = TRUE, title = 'Monthly arrivals across the Balkans and the Mediterranean Sea',
                          plotlyOutput("map"),
                                        sliderInput("slider.map", "Date", min = min(slider.range),
                                                    max = max(slider.range),
                                                    value=min(slider.range),
                                                    timeFormat = "%b %Y",
                                                    step = 31,
                                                    ticks = TRUE,
                                                    sep = ",",
                                                    animate = animationOptions(interval = 1500,
                                                    loop = FALSE)
                                                    ) ,
                          dygraphOutput("arrivals_by_day", width = '100%', height=150)
                      )
                            )
            ),
            tabItem(tabName = "stats",
                    fluidRow( 
                        column( width = 9,
                                box(width = "100%", height = '70%',status = "primary", solidHeader = TRUE, 
                                    collapsible = T, title = 'Country of origin and country of asylum',
                                    plotlyOutput("origin")),
                                box(width = "100%", height = '70%',status = "primary", solidHeader = TRUE, 
                                    collapsible = T, collapsed = F, title = 'Demographics per country of asylum',
                                    plotlyOutput("gender"))
                              ),
                        column( width = 3, 
                          box( width = NULL, status = "primary", solidHeader = TRUE, title = "Parameters",
                            helpText("Country of origin and gender distribution across migrants."), ## subtitle
                            selectInput("country",  # choose the countries
                                        label = "Choose a destination country to display",
                                        choices = countries,
                                        selected = "Greece")
                             ),
                          valueBox("241.263", 'Migrants in 2016', icon =
                                     icon("users"),width = NULL),
                          valueBox("> 1.7 M",'Migrants since 2008', icon = 
                                     icon("users"),width = NULL,color='orange'),
                          valueBox("~ 2.8 M ",'Syrian refugees in Turkey', icon = 
                                     icon("send"),width = NULL,color='green'),
                          valueBox("10.222", 'Estimated deaths', icon = 
                                     icon("warning"),width = NULL,color='red')
                              )
                    )
            )
    )))
)