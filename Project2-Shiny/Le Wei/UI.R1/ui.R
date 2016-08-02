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
        mainPanel(plotOutput("map"))
        )
))