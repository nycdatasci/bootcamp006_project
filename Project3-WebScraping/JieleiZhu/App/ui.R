library(shiny)

shinyUI(
  dashboardHeader(title = 'Better understand deals (Created by Emma(Jielei) Zhu)'),
  sidebarMenu(
    menuItem('About', tabName = "about", icon = icon("dashboard")),
    menuItem('What deals are popular?', tabName = "popular deals", icon = icon("dashboard")),
    menuItem('Which stores have popular deals?', tabName = "popular stores",icon = icon("dashboard")),
    menuItem('Which stores have the most deals?', tabName = "store most deals", icon = icon("th")),
    menuItem('When are there most deals?', tabName = "time most deals", icon = icon("th")),
    menuItem('Prediction', tabName = "prediction", icon = icon("th"))
  ),
  dashboardPage(skin = "black"),
  dashboardBody(
    tabItems(
      tabItem(
              tabItem(tabName = 'about',
                      fluidRow(
                        column(width = 2
                               
                        )
                      )
              ),
              tabItem(tabName = 'popular deals',
                      fluidRow(
                        style = "background-color:#F8F8F8",
                        column(width = 2,
                               selectInput('PopularCategory',
                                           'Select Category',
                                           choices = category_choices,
                                           selected = 'Baby'),
                               selectInput('PopularMetric',
                                           'Select Popularity Metric',
                                           choices = metric_choices,
                                           selected = 'Both')
                        ),
                        column(width = 8,
                               offset = 3,
                               h2('Most Popular Deals'),
                               dataTableOutput('TopDeal')
                               
                        )
                      )   
                      
              ),
              tabItem(tabName = 'popular stores',
                       fluidRow(
                         style = "background-color:#F8F8F8",
                         column(width = 2,
                                selectInput('PopularStoreCategory',
                                            'Select Category',
                                            choices = category_choices,
                                            selected = 'Baby'),
                                selectInput('MinimumDeals',
                                            'Select Minimum Number of Deals',
                                            choices = minimum_choices,
                                            selected = 5)
                         ),
                         column(width = 8,
                                offset = 3,
                                h2('Popular Stores'),
                                dataTableOutput('PopularStore')
                                
                         )
                       )   
                       
              ),
              tabItem(tabName = 'store most deals',
                       fluidRow(
                         style = "background-color:#F8F8F8",
                         column(width = 2,
                                selectInput('MostStoreCategory',
                                            'Select Category',
                                            choices = category_choices,
                                            selected = 'Baby')
                         ),
                         column(width = 8,
                                offset = 3,
                                h2('Stores with Most Deals'),
                                dataTableOutput('MostStore')
                                
                         )
                       )   
              ),
              tabItem(tabName = 'time most deals',
                       fluidRow(
                         # style = "background-color:#F8F8F8",
                         column(width = 2,
                                selectInput('MostDealCategory',
                                            'Select Category',
                                            choices = category_choices,
                                            selected = 'Baby')
                         )
                       ),
                       fluidRow(
                         column(width = 8,
                                offset = 3,
                                h2('Timeline'),
                                htmlOutput('timeline')
                         )
                       ),
                       fluidRow(
                         # style = "background-color:#F8F8F8",
                         column(width = 2,
                                selectInput('SingleMostCategory',
                                            'Select Category',
                                            choices = category_choices,
                                            selected = 'Baby'),
                                textInput.typeahead('SingleMostStore',
                                                    placeholder="Type store name here",
                                                    local=data.frame(name = store_names),
                                                    valueKey = "name",
                                                    tokens=c(1:length(store_names)),
                                                    template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")
                                )
                         ),
                         column(width = 8,
                                offset = 3,
                                h2('Timeline'),
                                htmlOutput('annotation')
                                
                         )
                       ) 
              ),
              tabItem(tabName = 'prediction',
                       fluidRow(
                         column(width = 8,
                                offset = 2,
                                h2('How to infer which deals are good deals?')
                                
                         )
                       )
              )
      )
    )
  )
)