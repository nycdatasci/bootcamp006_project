library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(rPython)
library(pdftools)

python.load("helperP3.py") 

ui = 
  dashboardPage(
    dashboardHeader(title = 'Dashboard'),
    dashboardSidebar(
      sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("info")),
        menuItem("Search Job", tabName = "search", icon = icon("database")),
        fileInput('file1', "Insert your CV as PDF file",
                  accept=c('pdf'))
        
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'about',
                fluidRow(
                  box(title = 'Web scraping Glassdoor.com: Will I ever find a job?', status = "primary", solidHeader = TRUE, collapsible = F, width = 12,
                      br(),
                      tags$p("Data scientists are lazy. We do not like browsing through a gazillion job posts,
                              we would rather the website to recommend the best jobs. Aren't they already doing 
                              it? Sure, but you have to sign in multiple websites, share your personal infos, 
                              set an alert, check the company reputation....Way too much work!"),
                      tags$p('The App reverses the usual approach, allowing users to upload their CV and 
                              match their relevant skills to the existing database. Job posts are scraped from 
                              Glassdoor.com using SELENIUM, focusing in particular on US market. The algorithm 
                              treats both the CV and the job post as unstructured texts, filter over 50 keywords 
                              and calculate the best match based on "Jaccard" similarity.'),
                      tags$p('For more information on the project or on the data, please visit the links below.'),
                      tags$hr(),
                      tags$a(href = ": http://blog.nycdatascience.com/student-works/web-scraping/glassdoor-webscraping/",
                             " Blog post: Web scraping Glassdoor - Land your dream job!", style = "font-size: 18px;"),
                      tags$br(),
                      tags$a(href = "http://www.glassdoor.com", 'Glassdoor.com', style = "font-size: 18px;")
                      ),
                  box(title = 'About the dataset: companies, locations and skills', status = "primary", solidHeader = TRUE, collapsible = T, collapsed = T, width = 12,
                  
                      splitLayout(cellWidths = c("50%", "50%"), tags$img(src = 'top10cities.png',width = '100%'), tags$img(src = 'top20employers.png',width = '100%')), #plotOutput("plotgraph1"), plotOutput("plotgraph2"))
                      splitLayout(cellWidths = c("50%", "50%"), tags$img(src = 'topSkills.png',width = '100%'), tags$img(src = 'education.png',width = '100%'))#plotOutput("plotgraph1"), plotOutput("plotgraph2"))
                  )),
                fluidRow(
                  box(title = "About me", status = "primary", solidHeader = TRUE, collapsible = T, collapsed = T,width = 12,
                      column(
                        width = 2,
                        br(),
                        br(),
                        tags$img(
                          src = 'photo2.jpg',
                          width = "100px", height = "100px"),
                        br(),
                        br(),
                        br(),
                        tags$a(href = "https://www.linkedin.com/in/diegodelazzari", icon("linkedin", "fa-2x")),
                        tags$a(href = "https://github.com/nycdatasci/bootcamp006_project/tree/master/Project3-WebScraping/DiegoDeLazzari/", icon("github", "fa-2x"), style = "margin-left: 20px;")
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
                        tags$p("This project was completed for the NYC Data Science Academy. More info at: "),
                        tags$a(href = "http://nycdatascience.com/", "NYC Data Science Academy", style = "font-size: 18px;")
                        )
                ))),
        tabItem(tabName = "search",
                fluidRow(
                  box(width = NULL, status = "primary", solidHeader = TRUE,
                      title = "Land your dream job on Glassdoor!",dataTableOutput("table"))
                        )
                )
            )
        )
      )

server = function(input, output){
  output$table <- renderDataTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    cvPdf = paste(pdf_text(inFile$datapath),collapse = '')
    python.call('get_bestMatch', cvPdf)
    
    f = read.csv('BestMatch.csv',
                 sep = ",",dec = ".", stringsAsFactors = F) 
    data = f%>% mutate(.,Link = sprintf('<a href="%s" target="_blank" class="btn btn-primary">Info</a>',Link))
    return(data[,-1])
  }, escape = FALSE )
}

shinyApp(ui, server)


# Example:
# cvPdf = pdf_text('DiegoCV.pdf')
# myCV = c('data scientist phd french python r matlab spark sql physics')
# 
# Load/run the main Python script
# python.load("helperP3.py") 
# Call the function and get the saved file
# python.call('get_bestMatch', cvPdf)
# f = read.csv('BestMatch.csv',
#              sep = ",",dec = ".", stringsAsFactors = F) 
# data = tbl_df(f)