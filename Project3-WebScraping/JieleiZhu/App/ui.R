library(shiny)

dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Where are the good deals? --Created by Emma(Jielei) Zhu",
    titleWidth = 600
  ),
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      menuItem(h4('About the data'), tabName = "about", icon = icon("dashboard")),
      menuItem(h4('WHAT DEALS are popular?'), tabName = "popular_deals", icon = icon("money")),
      menuItem(h4('Which stores have POPULAR deals?'), tabName = "popular_stores",icon = icon("shopping-bag")),
      menuItem(h4('Which stores have the MOST deals?'), tabName = "store_most_deals", icon = icon("shopping-basket")),
      menuItem(h4('WHEN are there most deals?'), tabName = "time_most_deals", icon = icon("shopping-cart")),
      menuItem(h4('Prediction'), tabName = "prediction", icon = icon("flask"))
    )),
  # dashboardPage(),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'about',
              fluidRow(
                column(width = 11,
                       offset = 1,
                       h1(
                         tags$ul(
                           tags$li('All data are crawled from ', 
                                   a(href="https://dealmoon.com", ' dealmoon.com')), 
                           br(),
                           tags$li("Total of ", span("~45,000", style = 'color:red'), 
                                   " deals from ", 
                                   span("8", style = 'color:blue'), " categories (i.e. ", 
                                   span("Clothing, Beauty, Nutrition, Baby, Home, Electronics, Travel, Finance", style='color:blue'), " )"), 
                           br(),
                           tags$li("Total of ", span("6", style = 'color:green'), " attributes (i.e.", span("category of deal, deal title, deal description, posted time, number of comments, number of bookmarks", style = 'color:green')),
                           br(),
                           tags$li("The entire crawling process took ~6hrs."),
                           br(),
                           img(src='dealmoon.png', height=330, width=800)
                         )
                       )
                )
              )
      ),
      tabItem(tabName = 'popular_deals',
              fluidRow(
                br(),br(),br(),
                column(width = 3,
                       selectInput('PopularCategory',
                                   h3('Select Category'),
                                   choices = category_choices,
                                   selected = 'Baby')
                ),
                column(width = 3,
                       selectInput('PopularMetric',
                                   h3('Select Popularity Metric'),
                                   choices = metric_choices,
                                   selected = 'By Bookmarks')
                ),
                box(
                  width = 12,
                  h2('Most Popular Deals'),
                  dataTableOutput('TopDeal')
                  
                )
              )
              
      ),
      tabItem(tabName = 'popular_stores',
              fluidRow(
                br(),br(),
                column(width = 3,
                       selectInput('PopularStoreCategory',
                                   h3('Select Category'),
                                   choices = category_choices,
                                   selected = 'Baby')
                ),
                column(width = 9,
                       selectInput('MinimumDeals',
                                   h3('Select Minimum Number of Deals a Store Must Have'),
                                   choices = minimum_choices,
                                   selected = 5)
                ),
                box(
                  width = 12,
                  h2('Popular Stores'),
                  dataTableOutput('PopularStore')
                  
                )
              )   
              
      ),
      tabItem(tabName = 'store_most_deals',
              fluidRow(
                column(width = 3,
                       br(),br(),
                       selectInput('MostStoreCategory',
                                   h3('Select Category'),
                                   choices = category_choices,
                                   selected = 'Baby')
                )
              ),
              fluidRow(
                box(width = 12,
                       h2('Stores with Most Deals'),
                       dataTableOutput('MostStore')
                )
              )
      ),
      tabItem(tabName = 'time_most_deals',
              fluidRow(
                column(width = 3,
                       selectInput('MostDealCategory',
                                   h3('Select Category'),
                                   choices = category_choices,
                                   selected = 'Baby')
                ),
                column(width = 6,
                       offset = 1,
                       h1('Deal Frequency By year', align = 'center')
                       )
              ),
              fluidRow(
                column(width = 9,
                       offset = 3,
                       htmlOutput('calendar')
                )
              ),
              br(),br(),br(),br(),br(),br(),
              fluidRow(
                box(width = 4,
                    selectInput('SingleMostCategory',
                                h3('Select Category'),
                                choices = category_choices,
                                selected = 'Baby',
                                width = '300px'),
                    textInput.typeahead('SingleMostStore',
                                        placeholder="Type store name here",
                                        local=data.frame(name = store_names),
                                        valueKey = "name",
                                        tokens=c(1:length(store_names)),
                                        template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")
                    )
                ),
                box(width = 8,
                    h2('Timeline'),
                    htmlOutput('annotation')
                )
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
      ),
      tabItem(tabName = 'prediction',
              fluidRow(
                column(width = 8,
                       offset = 2,
                       br(),br(),br(),br(),
                       h2("", align = 'center')
                       
                )
              )
      )
    )
  )
)
