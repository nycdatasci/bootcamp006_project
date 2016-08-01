library(shiny)
library(leaflet)
library(dplyr)
library(googleVis)
library(DT)

shinyUI(fluidPage(theme = "bootstrap.css",
            navbarPage("Pre-K Search in NYC", id='nav',
                       tabPanel("Pre-K Map",fluidPage(
                         leafletOutput("map",width = "100%",height=650),
                         absolutePanel(h4("Pre-K Finder"),id = "controls", class = "panel", fixed = TRUE,
                                       draggable = TRUE, top =80, left = 130, right = "auto", bottom = "auto",
                                       width = 180, height = "auto",
                                       checkboxGroupInput("borough", h5("Borough:"),
                                                          c('Bronx' = "Bronx",'Brooklyn' = "Brooklyn",
                                                            'Manhattan' = "Manhattan",'Staten Island' = "Staten Island",
                                                            'Queens' = "Queens"),selected=c('Bronx','Brooklyn',
                                                                                 'Manhattan','Staten Island',
                                                                                 'Queens')),
                                       checkboxGroupInput("type", h5("School Type:"),c('Charter'="Charter",
                                                                                       'DOE'="DOE",
                                                                                       'NYCEEC'='NYCEEC'),
                                                          selected=c("Charter","DOE","NYCEEC")),
                                       checkboxGroupInput("length", h5("Day Length:"),c("5-Hour" = "5-Hour" ,
                                                                                        "Full Day" = "Full Day",
                                                                                        "Full Day/5-Hour" = "Both Half Day and 5-Hour",
                                                                                        "Full/Half Day"= "Both Full and Half Day",
                                                                                        "Half Day/5-Hour"="Both Half Day and 5-Hour"),
                                                          selected=c("5-Hour" ,"Full Day","Both Half Day and 5-Hour",
                                                                     "Both Full and Half Day", "Both Half Day and 5-Hour")),
                                       actionButton("go", "Click to find"),
                                       helpText("Click on Pre-K Catalog on top for more detailed info")
                                       ))),
                       tabPanel("Pre-K Catalog",fluidPage(
                         fluidRow(
                           column(12,dataTableOutput('table')
                           )
                         ))),
                       tabPanel("Pre-K Statistics",fluidPage(
                         sidebarPanel(width =3,
                           checkboxGroupInput("boroughbar", h4("Borough:"),
                                              c('All Boroughs'="All Boroughs",'Bronx' = "Bronx",'Brooklyn' = "Brooklyn",
                                                'Manhattan' = "Manhattan",'Staten Island' = "Staten Island",
                                                'Queens' = "Queens"),selected=c('Bronx','Brooklyn','Manhattan',
                                                                                'Staten Island','Queens')),
                           checkboxGroupInput("yvar", h4("Attribute:"),c('Total Seats Available'="Seat Avalibility",
                                                                         'Est. Number of Kids in 2014'="2014 Estimate",
                                                                         'Number of Kids 2010 Census'="2010 Census"),
                                              selected= c("Seat Avalibility","2014 Estimate","2010 Census")
                           )      
                       ),
                       mainPanel(htmlOutput("bar"), p("Note: the numbers of 4-year-old children were derived by taking 20% of 
                                                       the total number of kids who are 0-5 years old. Hence, the number were  
                                                       only estimations."))
                       )),
                       tabPanel("About Universal Pre-K Program",h3("What is Universal Pre-K Program?"),
                                p("The aim of Mayor Bill de Blasio’s Universal Pre-K is to make access to FREE 
                                   pre-kindergarten education available to all NYC families, regardless of 
                                   child's abilities and family income."),
                                h3("Eligibility"),p("Universal Pre-Kindergarten Programs are available 
                                for children who turn 4 years old in the school year and live in New York City."),
                                h3("Pre-K Options"),
                                tags$ul(
                                  tags$li("NYCDOE Pre-K Centers: Programs dedicated exclusively to pre-K students that are led and operated by NYCDOE staff."),
                                  tags$li("NYCDOE District Schools: Programs located within public elementary schools and overseen by the school’s principal."),
                                  tags$li("NYC Early Education Centers (NYCEECs): Community-based organizations that contract with the NYCDOE through a competitive
                                          process to provide free pre-K programs."),
                                  tags$li("Charter Schools: Public schools that operate under a state-authorized charter, independent of NYCDOE regulations.
                                          Contact charter schools directly to apply to their pre-K programs.")),
                                h3("Application"),
                                p("From mid-March to late-April, parents can file pre-k application online or in person.Parents can rank up to 12 pre-k choices on the 
                                  application, both at public schools and community-based early education centers. Each child is matched with one program and acceptance 
                                  letters will go out in late May and parents can register in June.")
                                ),
                       tabPanel("About This Site",h3("Motivation"),p("This website is created for two purposes. 
                                                                     First, Pre-K map and catalog provide a comprehensive 
                                                                     guide for parents who do not understand Universal Pre-K Program 
                                                                     or simply want to quickly find the closest and most suitablepre-K 
                                                                     for their children. This Pre-K guide gives information
                                                                     about each pre-k’s location, contacts, meal plans, playspace, 
                                                                     extended day care options, and enrollment restrictions. Also, 
                                                                     this website allows the public to visualize whether there are enough 
                                                                     seats for eligible kids in each borough. "), h3("Data Source:"),tags$ul(
                         tags$li(a(href="https://data.cityofnewyork.us/Education/Universal-Pre-K-UPK-School-Locations/kiyv-ks3f",
                                   "Universal Pre-K Directory")), 
                         tags$li(a(href="http://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml",
                                   "American Fact Finder by United States Census Bureu"))),
                       h3("Related Articles"),tags$ul(
                         tags$li(a(href="http://schools.nyc.gov/ChoicesEnrollment/PreK/default.htm",
                                   "Pre-Kindergarten - NYC Department of Education")),
                         tags$li(a(href="http://theweek.com/articles/639370/early-childhood-education-matters--heres-how-make-great",
                                   "Early childhood education matters — here's how to make it great")),
                         tags$li(a(href="http://www.nytimes.com/2016/02/14/opinion/sunday/how-new-york-made-pre-k-a-success.html?_r=0",
                                   "How New York Made Pre-K a Success")),
                         tags$li(a(href="http://newyork.cbslocal.com/2016/05/09/universal-pre-k-complaints/",
                                   "City Parents Disgruntled Over De Blasio’s Pre-K Program, Some Forced To Travel An Hour To Get To School")), 
                         tags$li(a(href="http://www.wsj.com/articles/data-show-some-pre-k-programs-a-de-blasio-signature-initiative-miss-key-metric-1450461760",
                                   "New York City’s Pre-K Gets Mixed Reviews"))),h3("Contact Author"),p("Feel free to send questions and suggestions to amy17519@gmail.com.
                                                                                                        You can also find other projects by her at",
                                                                                                        a(href="http://blog.nycdatascience.com/author/amy17519/","Blog"),
                                                                                                        " or ", a(href="https://github.com/amy17519","Github"), "."))
  
)))
  
  
  
