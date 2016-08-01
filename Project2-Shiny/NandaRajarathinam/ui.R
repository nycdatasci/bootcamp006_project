library(shiny)
library(googleVis)

shinyUI(
  navbarPage(
    title = "U.S. Retail Banking Consumer Complaints",
    theme = shinytheme("flatly"),
    
    ### Welcome tab with important information
    tabPanel("Welcome",
             mainPanel(
               div(style = "margin-left: 15%;",
                   br(),
                   br(),
                   h3(strong("Welcome to the Consumer Complaints Analysis Shiny App"), style = "margin-top:0"),
                   br(),
                   p("The following app helps visualize the trend in retail banking related consumer complaints, 
                     breaking down information by product, financial institution and location."),
                   br(),
                   h4(strong("What is CFPB? What do they do?")),
                   p("The Consumer Financial Protection Bureau (CFPB) is a government agency that was created as a part of the Dodd-Frank 
                      Wall Street Reform act after the 2008 financial crisis to protect consumers. Their aim is to make consumer financial markets work for consumers,
                     responsible providers, and the economy as a whole. CFPB protects consumers from unfair, deceptive, or abusive practices 
                     and take action against companies that break the law. "),
                   br(),
                   h4(strong("What is a Consumer Complaint Database?")),
                   p("Consumer Financial Protection Bureau(CFPB) sends thousands of consumers' complaints about financial products and 
                     services to companies for response. Those complaints are published and stored in a Database after the company
                     responds or after 15 days, whichever comes first. By adding their voice, consumers help improve the financial 
                     marketplace. "),
                   br(),
                   h4(strong("Where was the data obtained?")),
                   p("The data was obtained from the", a("Consumer Financial Protection Bureau.", 
                                                         href="http://www.consumerfinance.gov/data-research/consumer-complaints/")),
                   br(),
                   h4(strong("I would like to explore more...")),
                   p("Great, click on another tab and use the Shiny App to inspect these trends. Try to 
                     uncover interesting trends."),
                   br(),
                   p(strong("Enjoy your visit!"))
                   )
                   )
               ),
    
    ### Tab allowing insight into volume of complaints trend
    tabPanel("Insight: Complaints Trend",
             sidebarLayout(
               sidebarPanel(h3("Parameters"), style = "width:400px",
                            helpText("This graph displays the trend in the number of complaints filed
                            based on the financial products or services offered by the firms. Select the products 
                                      to compare the volume trend over time."),
                            br(),
                            checkboxGroupInput("productCG", h4("Financial Product / Service:"), 
                                               choices = productlist, selected = "Bank account or service")
                            #, radioButtons("responseInsightRB", h4("Response Metric:"), choices = metricinsightlist)
                            
                            
                            ),
               mainPanel(style="position:relative",
                         h2("Volume of Complaints", style="margin:20px; margin-left:200px; z-index:100; position:absolute" ),
                         #h2(textOutput("txtResponseInsight"), style="margin:20px; z-index:100; margin-left:150px; position:absolute"),
                         htmlOutput("plot", height = "450", width = "500") #  , style="margin-left:-100px; margin-top:-80px")
                         
                         #htmlOutput("plot")
               )
               )
    ),
    
    ### Tab allowing insight into Response trend
    tabPanel("Insight: Response Trend",
             sidebarLayout(
               sidebarPanel(h3("Parameters"), style = "width:400px",
                            helpText("This graph displays the trend in the responses
                                     based on the financial products selected. Select the products 
                                     to compare the trend in delayed, disputed and responses with monetary reliefs over time."),
                            br(),
                            checkboxGroupInput("productInsightCG", h4("Financial Product / Service:"), 
                                               choices = productlist, selected = "Bank account or service"),
                            radioButtons("responseInsightRB", h4("Response Metric:"), choices = metricinsightlist)
                            
                            
                            ),
               mainPanel(style="position:relative",
                         #h2("Volume of Complaints", style="margin:20px; margin-left:200px; z-index:100; position:absolute" ),
                         #h2(textOutput("txtResponseInsight"), style="margin:20px; z-index:100; margin-left:95px; position:absolute"),
                         h2(textOutput("txtResponseInsight"), align = "center"),
                         htmlOutput("plotInsight", height = "450", width = "500", align ="center")
                         #htmlOutput("plotInsight", height = "450", width = "500" , style="margin-left:-100px; margin-top:20px")
                         
               )
               )
  ),
    
    ### Tab allowing insight into Top 5 Issues for each product
    tabPanel("Insight: Top Issues",
             sidebarLayout(
               sidebarPanel(h3("Parameters"), style = "width:400px",
                            helpText("This graph displays top 5 issues faced by consumers for a given product
                                     and year. Select the product and year to view the top 5 ranked
                                     complaints based on the number of cases filed."),
                            br(),
                            selectInput("prodSI", h4("Product/Service:"), choices = productlist, selected = "Bank account or service"),
                            selectInput("yearSI", h4("Year"), choices = yearlist,
                                        selected = "2012")

                            ),
               mainPanel(style="position:relative",
                         #h2("Top 5 Issues", style="margin:20px; margin-left:250px; z-index:100; position:absolute"),
                         #h2(textOutput("txtTopIssue"), align = "center"),
                         h2(textOutput("txtTopIssue"), style="margin:20px; z-index:100; margin-left:150px; position:absolute"),
                         #htmlOutput("plotTopIssue", align ="center")
                         htmlOutput("plotTopIssue", height = "550", width = "800", style="margin-left:-50px; margin-top:30px")
               )
               )
    ),  
    
  
    ### Tab allowing insight into  Dispute % for each  product  and tier
    tabPanel("Insight: Companies",
             sidebarLayout(
               sidebarPanel(h3("Parameters"), style = "width:400px",
                            helpText("This graph displays % of responses that were delayed or disputed by the consumers for a given product
                                     and tier. Select the product and tier to view the % of 
                                      disputed or delayed responses for the companies that fall within that tier."),
                            br(),
                            selectInput("prodResponseMetricSI", h4("Product/Service:"), choices = productlist, selected = "Bank account or service"),
                            selectInput("tierResponseMetricSI", h4("Tier"), choices = tierlist, selected = "Tier 1"), 
                            selectInput("yearResponseMetricSI", h4("Year"), choices = yearlist, selected = "2012"),
                            radioButtons("responseRB", h4("Response Metric:"), choices = metriclist)
                            
                            ),
               mainPanel(style="position:relative",
                         
                         h2(textOutput("txtResponseMetric"), style="margin:20px; z-index:100; margin-left:150px; position:absolute"),
                         #--htmlOutput("plotResponseMetric", height = "550", width = "900", style="margin-left:-100px; margin-top:30px")
                         htmlOutput("plotResponseMetric", height = "550", width = "800", style="margin-left:-50px; margin-top:30px")
                         
                         
               )
               )
    ) , 
    
    
    ### Tab showing volume of complaints, Disputed responses per state
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(h3("Parameters"), style = "width:400px",
                            helpText("This graph displays some of the key metrics reviewed by the Consumer Financial Protection Bureau (CFPB).
                  This includes the volume of complaints, percentage of delayed responses and disputed responses for the selected product and year. 
                                     Hover over each state to get more detailed information."),
                            br(),
                            #selectInput("changeS", h4("Calculation:"), choices = change), 
                            #br(),
                            selectInput("prodMapSI", h4("Product/Service:"), choices = productlist, selected = "Bank account or service"),
                            #selectInput("tierResponseMetricSI", h4("Tier"), choices = tierlist, selected = "Tier 1"), 
                            selectInput("yearMapSI", h4("Year"), choices = yearlist, selected = "2012"),
                            radioButtons("responseMapRB", h4("Metric:"), choices = metricmaplist, selected = "Volume of Complaints")
                            ),
               mainPanel(style="position:relative",
                         h2(textOutput("txtMap"), style="margin:20px; z-index:100; margin-left:80px; position:absolute"),
                         htmlOutput("plotMap", style="margin:-15px; margin-left:-50px; padding:0")
               )
             )
    )
    
    
    
    

    
 
    )
    )