library(shinydashboard)

#### Title ####
header <- dashboardHeader(
  titleWidth = 200, title = "Food Explorer")

#### Sidebar ####
sidebar <- dashboardSidebar(
  width = 200,
  
  # Logo
  sidebarUserPanel("Jonathan Liu", image = "donuts.gif"),
  
  # Tabs
  sidebarMenu(
    menuItem("Overview", tabName = "tab_map", 
             icon = icon("map")),
    menuItem("Explorer", tabName = "tab_plan", 
             icon = icon("apple", lib = "glyphicon")),
    menuItem('Summary', tabName = 'tab_summary', 
             icon = icon("star")),
    br(),
    verbatimTextOutput('summary_statVB'),
    br(),
    fluidRow(align = 'center',
    actionButton(inputId = 'summary_reset', 
                 label = 'Reset Selections',
                 icon('refresh'),
                 style = "color: #fff; 
                 background-color: #337ab7; 
                 border-color: #2e6da4;
                 height: 60px")),
    br(),
    menuItem('About', tabName = 'tab_about',
             icon = icon('info-circle'))
  )
)

#### Tabs - About ####
tab_about <- tabItem(
  tabName = 'tab_about',
  fluidRow(
    tabBox(
      width = 12,
      title = tagList(shiny::icon("map-o"), 'Overview'),
      id = 'tabbox1',
      tabPanel(
        "About World Food Fact Data",
        fluidRow(
          column(
            width = 3,
            tags$img(src = 'openfoodfacts-logo.png', width = "178px", height = "150px")),
          column(
            width = 9,
            tags$p("Open Food Facts is a food products
                     database made by everyone, for everyone."),
            tags$p("You can use it to make better food choices, 
                     and as it is open data, anyone can re-use it for any purpose."),
            tags$a(href = 'http://world.openfoodfacts.org/discover',
                   'Learn more about Open Food Facts'))
        )), # End of Tab Panel 1
      tabPanel(
        "About Me",
        fluidRow(
          box(
            width = 12, title = ,
            column(
              width = 2,
              tags$img(
                src = 'JonathanLiu.jpg',
                width = "100px", height = "100px")),
            column(
              width = 10,
              tags$p(aboutme),
              br(),
              br(),
              tags$p(icon('github-alt'), 
                     tags$a(href = 'https://github.com/jonathanlxy',
                            'My Github')),
              tags$p(icon('linkedin'), 
                     tags$a(href = 'https://www.linkedin.com/in/jonathanlxy',
                            'My LinkedIn'))
            )))) # End of Tab Panel 2
    ))) # End of Tab Box
               
#### Tabs - Map ####
tab_map <- tabItem(
  tabName = "tab_map", 
  # Row 1, Region Selection
  
  fluidRow(
    column(9, align = 'center',
           selectInput(inputId  = 'Map_RegionSelection',
                       label    = 'Select Region to Display',
                       choices  = region_geo$Region,
                       selected = 'World')
    ),
    column(3, align = 'left',
           valueBox(
             value = dim(country_stat)[1],
             subtitle = "Countries Available",
             icon = icon("globe", lib = "glyphicon"),
             width = 16)
    )
  ),
  
  # Row 2, Map of Region + Country List
  fluidRow(
    box(
      title = 'World Map', status = 'primary', solidHeader = T,
      height = 580, width = 9,
      htmlOutput("map_map")
    ), # End of Map Box
    box(
      title = 'Country Summary', status = 'primary', 
      solidHeader = T, height = 580, width = 3,
      dataTableOutput('map_country')
    )  # End of Info Box
  )
  
) # End of Map Tab

#### Tabs - Plan ####
tab_plan    <- tabItem(
    tabName = "tab_plan",
    
    #### Country Selection
    fluidRow(
      selectInput(inputId = 'plan_RegionSelection',
                  label = 'Select Your Destination',
                  choices = country_stat$Country),
      align = 'center'),
    
    #### Nutrition Filters
    fluidRow(
      box(width = 12, align = 'center', 
        collapsible = T, collapsed = T,
        status = 'info', solidHeader = T,
        title = 'Nutrition Filters',
        
        # Checkgroup
        column(2, align = 'left',
               helpText("Derply Derp Nyan Nyan Nyan"),
               checkboxGroupInput(
                 inputId = "plan_CheckGroup", 
                 label = h3("Checkbox group"),
                 choices = nutritions,
                 selected = nutritions[1]
               )
        ), # End of Checkgroup
        
        # Sliders col 1
        column(5, 
               lapply(1:5, function(i) {
                 sliderInput(inputId = paste0('plan_', nutritions[i]), 
                             label   = nutritions[i],
                             min = min_v[nutritions[i]],
                             max = max_v[nutritions[i]],
                             value = c(min_v[nutritions[i]],
                                       max_v[nutritions[i]])
                 )
               })
        ), # End of Slider col 1
        
        # Sliders col 2
        column(5,
               lapply(6:10, function(i) {
                 sliderInput(inputId = paste0('plan_', nutritions[i]), 
                             label   = nutritions[i],
                             min = min_v[nutritions[i]],
                             max = max_v[nutritions[i]],
                             value = c(min_v[nutritions[i]],
                                       max_v[nutritions[i]])
                 )
               })
        )  # End of Slider col 2
      )  # End of Filter Box 
    ), # End of Filter Row
    
    # Stats
    htmlOutput('plan_statVB'),
    
    # Items DT
    fluidRow(
      box(status = 'primary', solidHeader = T,
          title = 'Matching Items', width = 12,
        dataTableOutput('plan_displayDT')
        )
      )
) # End of Plan Tab

#### Tabs - Summary ####
tab_summary <- tabItem(
  tabName = 'tab_summary',
  htmlOutput('summary_infoUI'),
  
  fluidRow(
    box(status = 'primary', solidHeader = T,
        collapsible = T, collapsed = T,
        title = 'Selection vs. DV - Detail', width = 12,
        dataTableOutput('summary_avgDT'))),
  
  # fluidRow(
  #   column(12, align = 'right',
  #          
  #   )),
  

  fluidRow(
    box(status = 'primary', solidHeader = T,
        title = 'Detail', width = 12,
        dataTableOutput('summary_DT')))
) # End of summary Tab

#### Dashboard Body ####
body <- dashboardBody(
  ### Tabs
  tabItems(
    tab_map,    # Map tab
    tab_plan,   # Plan tab
    tab_summary, # Summary tab
    tab_about  # About tab
  ) # End of Tabs
) # End of Body

#### Main Frame ####
dashboardPage(
  # Properties
  skin = "blue",
  
  # Main Frame
  header,   
  sidebar,
  body
)
