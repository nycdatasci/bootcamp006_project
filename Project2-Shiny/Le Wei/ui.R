library(shiny)

regions <- c("World",
             "Asia",
             "Africa",
             "Europe",
             "NorthAmerica")

types <- c('Total.population..thousands.', 
           'Birth.rate','Mortality.rate',
           'Life.expectancy', 
           'Infant.mortality.rate',
           'Growth.rate',
           'Population.aged.65.and.more..thousands.')

shinyUI(fluidPage(
    titlePanel("World Census"),   # main title
    navbarPage('World Problems', id = 'WP',
    tabPanel('Main Page',
      sidebarLayout(
        sidebarPanel(
          img(src="http://blog.loukavar.com/wp-content/uploads/2012/01/world.jpg", height = 200, width = 200),
            helpText("Create demographic maps with
                     information from the 2016 World Census."), ## subtitle
            
          selectInput("region",  # choose the residents
                        label = 'Region',
                        choices = regions),
            selectInput("type",
                        label = "Type",
                        choices = types)
         
        ),
        mainPanel(plotOutput("map")
        ))),
    tabPanel('Data Source',
             column(6, img(src="picture1.jpg", height=400, width=400)),
             column(6,p("The data was provided by UN in 2016. it includes the data worldwide about Birth rate, mortality rate, population
            ,Life Expectancy,Infant Mortality rate,Population above 65 years old."),
            p("Analyze data like this hope to help which countries suffer what kind of problems
                 , in order for UN to start helping the countries which has the most serioues problem"),
            p("Unfortunately, UN provided data except central and south America, and some other countries."))
            ),
    tabPanel('Definition',
             p('Total population calculate the population in thousands'),
             p('Birth rate means the number of children born per 1,000 of population'),
             p('Mortality rate indicates the number of people died per 1,000 of population'),
             p('Infant mortality rate means number of infant died per 1000 child birth'),
             p('Population aged 65 and more is calculated in thousands'))
)))