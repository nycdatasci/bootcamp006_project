library(shiny)
library(ggplot2)
library(dygraphs)
library(stringr)
library(scales)

shinyUI(
  navbarPage(
    title = "U.S. Travel and Transport Analysis",
    theme = shinytheme("flatly"),
    
    ### Welcome tab with important information
    tabPanel("Welcome",
             mainPanel(
               div(style = "margin-left: 15%;",
                   br(),
                   br(),
                   h3(strong("Welcome to the Travel Analysis Shiny App"), style = "margin-top:0"),
                   br(),
                   p("The U.S. Department of Commerce announced that 5.5 million international visitors traveled to the United States in January 2016.January 2016 registered the fifth straight month of increases in total U.S. visits."),
                   br(),
                   h4(strong("Why this app?")),
                   p("The app analyzes the foreign travellers travelling to United States based on country, purpose of visit and port of entry "),
                   br(),
                   h4(strong("Where was the data obtained?")),
                   p("The data was obtained from the", a("United States Travel Trade Government Data.",
                                                         href="http://travel.trade.gov/research/monthly/arrivals/index.asp")),
                   br(),
                   h4(strong("How are projections made?")),
                   p("Projections are made based on the January 2016 data.More information can be found ",
                     a("here.", href="http://travel.trade.gov/")),
                   br(),
                   #h4(strong("This is boring.")),
                   p("Great, click on another tab and use the Shiny App to inspect these projections. Try to
                     uncover interesting trends.")
                   #br(),
                   #p(strong("Enjoy your visit!"))
                   )
                   )
               ),
    
    tabPanel("People Visiting USA",
             sidebarLayout(
               sidebarPanel(
                 helpText("Select either Top 20 Countries visiting US or North American Countries visiting US"),
                 radioButtons("radio", label = h2("Select an input"),
                              choices = list("North America" = "nam", "Top 20 Countries" = "top"),
                              selected = 1)

               ),
               mainPanel(style="position:relative",
                         h2("International Vistors From Different Countries "),
                         br(),
                         htmlOutput("plot_tn", width = "100%")
               )
             )
    ),
    
    tabPanel("World",
             sidebarLayout(
               sidebarPanel(
                 helpText(".	In January 2016, the top 20 inbound visitor markets accounted for 88 percent of all international arrivals to the United States  ")
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Table", htmlOutput("plot_wd")),
                   tabPanel("Plot", htmlOutput("plot_wdh"))
                 )

               )
             )
    ),
    
    tabPanel("Business travel vs Pleasure travel",
             # sidebarLayout(
             #   sidebarPanel(
             #     helpText("Business Traveller vs Pleasure Traveller across the top  20 countries travelling to ")
             # 
             #     ),
             # br(),
             #   mainPanel(style="position:relative",
             #             helpText("Business Traveller vs Pleasure Traveller across the top  20 countries travelling to "),
             #            # h2("Projected Population", style="margin:20px; margin-left:200px; z-index:100; position:absolute"),
             #             htmlOutput("plot_bp")
             #   )
             # )
             basicPage(
               fluidRow(
                 column(11,
                        htmlOutput("plot_bp"))
               )
             )
    ),

    tabPanel("Import vs Export",
             sidebarLayout(
               sidebarPanel(
                 helpText("Import vs Export of the USA"),
                 selectInput("select", label = h3("Select box"),
                             choices = list("Total Travel and Tourism Related Trade" = 2, "Total Payment" = 3, "Travel Spending3" = 4, "Medical/Education/Workers Spending4" = 5, "Passenger Fare" = 6),
                             selected = 1),
                 dateRangeInput("dates", label = h3("Date range"), start = min(z$Date), end = max(z$Date))
                 
                 ),
               mainPanel(style="position:relative",
                         plotOutput("plot_ie")
               )
             )
    ),
    tabPanel("Top Ports",
             sidebarLayout(
               sidebarPanel(
                 helpText("Top Ports For Arrival")
                 
               ),
               mainPanel(style="position:relative",
                         h2("Top Ports"),
                         br(),
                         htmlOutput("plot_tp", style="margin-left:-100px; margin-top:-80px")
               )
             )
    )
  )
)
    
