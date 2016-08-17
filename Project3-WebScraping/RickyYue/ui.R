shinyUI(
  navbarPage(
    theme = "slate.css",
    title = "Crawling in a Plastic Jungle",
    #theme = shinytheme("flatly"),
    tabPanel(
      "Welcome to the Plastic Jungle",
      mainPanel(
        div(style = "margin-left: 5%;",
            br(),
            br(),
            h3(strong("Watch your step in the plastic jungle"), style = "margin-top:0"),
            img(src="pj_crawler.jpg",height=200,width=500),
            br(),
            p("This Shiny app helps visualize the supply in discounted gift cards from",a("www.cardpool.com,",href="http://www.cardpool.com"), 
            "one of the major gift card exhcnage websites, on Saturday 8/13/2016."),
            br(),
            hr(),
            h4(strong("Why interested in a gift card exchange website?")),
            p("People love discounts."),
            p("What cards are easy to get?"),
            p("What cards are short in supply?"),
            p("More off, more popular?"),
            br(),
            hr(),
            h4(strong("How was the data obtained?")),
            p("Since the availability of gift cards on the exhange website are changing by time, the basic idea is to scrape the 
              website every a few hours on a given day so the dynamic change of supply in gift cards within that day is monitored. 
              However not every scheduled scraping worked, it ended up with scraping the full site at 2 am, 5 am, 7 am, 8 am, 
              9 am, 2 pm, 3 pm, 5 pm, 8 pm, 9pm and 11pm(EST) on Saturday, 8/13/2016."),
            br(),
            hr(),
            h4(strong("The Data:")),
            p("A list with names of all gift cards that had been on sale on Cardpool until 8/13/2016 were first obtained. For every 
               card available at the scheduled scraping time, the merchant name, the category it belongs to, the value, the 
               price for sale, the percentage of discount, the card type(physical, electronic, mobile) were scraped, and the scraping 
               time for that specific card (H:M:S) was also documented."),
            br(),
            hr(),
            h4(strong("Let's start exploring the plastic jungle...")),
            br(),
            br()
            )
            )
    ),
    tabPanel("Likert Scale by Category",
             sidebarLayout(
               sidebarPanel(style = "width:225px",
                            selectInput("cat1",label=h4("Category:"), choices=c(unique(df_avail$category),"All"),selected="All",multiple=TRUE)
                            ),
               mainPanel(style="position:relative",
                         plotOutput("myLikertChart",height=700,width=800)
               ),
               position = "right", fluid = TRUE
               )
             ),
    tabPanel("Time Series by Category",
             sidebarLayout(
               sidebarPanel(style = "width:200px",
                            selectInput("type2",label=h4("Card Type"), choices=as.character(unique(df$type)),selected="physical"),
                            selectInput("cat2",label=h4("Category"), choices=c(unique(df_avail$category),"All"),selected="All",multiple=TRUE)
                            ),
             mainPanel(
             plotlyOutput("myCountChart1"),
             plotlyOutput("myValueChart1"),
             width=8
                       ),
             position = "right", fluid = TRUE)
             ),
    tabPanel("Time Series by Merchant",
             sidebarLayout(
               sidebarPanel(style = "width:200px",
                            selectInput("type3",label=h4("Card Type"), choices=as.character(unique(df$type)),selected="physical"),
                            selectInput("cat3",label=h4("Category"), choices=unique(df_avail$category)),
                            uiOutput("myCatSelection")
               ),
               mainPanel(
                 plotOutput("myCountChart2"),
                 plotOutput("myValueChart2")
                 ##width=8
               ),
               position = "right", fluid = TRUE)
    ),
    tabPanel("Lookup Table",
             sidebarLayout(
               sidebarPanel(style = "width:200px",
                            selectInput("type4",label=h4("Card Type"), choices=as.character(unique(df$type)),selected="physical")
               ),
               mainPanel(
                 dataTableOutput("myTable"),
                 width=8
               ),
               position = "right", fluid = TRUE)
    ),
    tabPanel("Scatter Plot",
             sidebarLayout(
               sidebarPanel(style = "width:200px",
                            selectInput("type5",label=h4("Card Type"), choices=as.character(unique(df$type)),selected="physical")
               ),
               mainPanel(
                 plotlyOutput("my2dChart"),
                 width=8
               ),
               position = "right", fluid = TRUE)
    )
  )
)
