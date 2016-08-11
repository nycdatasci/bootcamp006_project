library(shinydashboard)

#### Title ####
header <- dashboardHeader(
  titleWidth = 200, 
  title = tagList(shiny::icon("grain", lib = "glyphicon"), 
                  "Food Explorer"))

#### Sidebar ####
sidebar <- dashboardSidebar(
  width = 200,
  
  # Logo
  sidebarUserPanel("Jonathan Liu", image = "donuts.gif"),
  
  # Tabs
  sidebarMenu(
    menuItem('About', tabName = 'tab_about',
             icon = icon('info-circle')),
    menuItem("Overview", tabName = "tab_map", 
             icon = icon("map")),
    menuItem("Explorer", tabName = "tab_plan", 
             icon = icon("apple", lib = "glyphicon")),
    # menuItem('Summary', tabName = 'tab_summary', 
    #          icon = icon("star")),
    br(),
    verbatimTextOutput('summary_statVB')
  )
)

#### Tabs - About ####
tab_about <- tabItem(
  tabName = 'tab_about',
  fluidRow(
    tabBox(
      width = 12,
      title = tagList(shiny::icon("info-circle"), 'Overview'),
      id = 'tabbox1',
      tabPanel(
        "About World Food Fact Data",
        fluidRow(
          column(
            width = 5, align = 'center',
            tags$img(src = 'openfoodfacts-logo.png', width = "356px", height = "300px")),
          column(
            width = 7,
            tags$h3('Open Food Facts is a food products
                    database made by everyone, for everyone. 
                    You can use it to make better food choices, 
                    and as it is open data, anyone can re-use it for any purpose.'),
            tags$h3('This Shiny App is built as a user-friendly replacement for 
                    original search interface on the Open Food Facts website.'),
            tags$a(href = 'http://world.openfoodfacts.org/discover',
                   'Learn more about Open Food Facts',
                   class="btn btn-primary"))
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
              tags$p(tags$a(icon('github-alt'), target = '_blank',
                            'My Github',
                            href = 'https://github.com/jonathanlxy',
                            class="btn btn-primary")),
              tags$p(
                     tags$a(icon('linkedin'), target = '_blank',
                            'My LinkedIn',
                            href = 'https://www.linkedin.com/in/jonathanlxy',
                            class="btn btn-primary"))
            )))) # End of Tab Panel 2
    ))) # End of Tab Box
               
#### Tabs - Map ####
tab_map <- tabItem(
  tabName = "tab_map", 
  # Row 1, Region Selection
  fluidRow(
    column(6, align = 'justify',
           box(title = 'About This Tab', width = 13,
               status = 'info', solidHeader = T,
               'This overview dashboard shows the number of products included
               in the Open Food Facts database, summarized by each country.'
               )
           ),
    column(3, align = 'center',
           box(title = 'Select Region to Display', width = 12, height = 100,
               status = 'info', solidHeader = T,
               selectInput(inputId  = 'Map_RegionSelection',
                       label    = NULL,
                       choices  = region_geo$Region,
                       selected = 'World'))
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
      height = 590, width = 9,
      htmlOutput("map_map")
    ), # End of Map Box
    box(
      title = 'Country Summary', status = 'primary', 
      solidHeader = T, height = 590, width = 3,
      dataTableOutput('map_country')
    )  # End of Info Box
  )
  
) # End of Map Tab

#### Tabs - Plan ####
tab_plan    <- tabItem(
    tabName = "tab_plan",
    
    #### Country Selection
    fluidRow(
      box(
        title = ('Select Your Country'), width = 12,
        status = 'info', solidHeader = T,
      selectInput(inputId = 'plan_RegionSelection',
                  label = NULL,
                  choices = country_stat$Country),
      align = 'center')),
    
    #### Nutrition Filters
    fluidRow(
      box(width = 12, align = 'center', 
        collapsible = T, collapsed = T,
        status = 'info', solidHeader = T,
        title = tagList(shiny::icon("sliders"), 'Select Nutritional Items'),
        
        # Checkgroup
        fluidRow(
          column(1),
          column(10,
                 checkboxGroupInput(
                   inputId = "plan_CheckGroup", 
                   label = h3("Step 1 - Select Nutritional Items"),
                   choices = nutritions,
                   selected = NULL, inline = T
                 )),
          column(1)
          
          # column(1, actionButton(inputId = 'filter_reset', 
          #                        label = NULL,
          #                        icon('refresh'),
          #                        style = "color: #fff; 
          #        background-color: #337ab7; 
          #        border-color: #2e6da4"
          # ))
          
        ), # End of Checkgroup
        helpText('("_100g" means "per 100 grams of food")'),
        # Sliders col 1
        htmlOutput('plan_filterUI') # End of Slider Row
      )  # End of Filter Box 
    ), # End of Filter Row
    
    
    
    # Items DT
    fluidRow(
      box(status = 'primary', solidHeader = T,
          collapsible = T, collapsed = F,
          title = tagList(shiny::icon("table"), 'Matching Items'), 
          width = 12,
          # Stats
          htmlOutput('plan_statVB'),
          dataTableOutput('plan_displayDT')
        )
      ),
    
    # Plot Box
    fluidRow(
      box(
        width = 12, height = 'auto', align = 'center', 
        collapsible = T, collapsed = T,
        status = 'primary', solidHeader = T,
        title = tagList(shiny::icon("bar-chart"), 
                        'Distribution of Nutritional Items'),
        
        fluidRow(
          column(
            width = 12,
            selectInput(inputId  = 'map_plotX',
                        label    = 'Select Nutritional Item',
                        choices  = nutritions,
                        selected = nutritions[1]))
        ),
        fluidRow(
          column(1),
          column(
            10, plotOutput('plot')
          ),
          column(1)
        ))), # End of Plot Box
    
    h2('Summary of Selections'),
    htmlOutput('summary_infoUI'),
    
    
    fluidRow(
      box(status = 'primary', solidHeader = T,
          collapsible = T, collapsed = T,
          title = 'Selection vs. DV - Detail', width = 12,
          dataTableOutput('summary_avgDT'))),
    
    fluidRow(
      box(status = 'primary', solidHeader = T,
          title = 'Selection Detail', width = 12,
          dataTableOutput('summary_DT')))
    
) # End of Plan Tab

#### Tabs - Summary ####
# tab_summary <- tabItem(
#   tabName = 'tab_summary',
#   htmlOutput('summary_infoUI'),
#   
#   fluidRow(
#     box(status = 'primary', solidHeader = T,
#         collapsible = T, collapsed = T,
#         title = 'Selection vs. DV - Detail', width = 12,
#         dataTableOutput('summary_avgDT'))),
# 
#   fluidRow(
#     box(status = 'primary', solidHeader = T,
#         title = 'Detail', width = 12,
#         dataTableOutput('summary_DT')))
# ) # End of summary Tab

#### Dashboard Body ####
body <- dashboardBody(
  ### Tabs
  tabItems(
    tab_map,    # Map tab
    tab_plan,   # Plan tab
    # tab_summary, # Summary tab
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
