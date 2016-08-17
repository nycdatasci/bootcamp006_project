library(shiny)
library(plotly)
library(dplyr)
library(countrycode)

shinyUI(fluidPage(theme = "bootstrap.css",
                  navbarPage("The World of Starbucks Muggers", id='nav',
                             tabPanel("Geography",fluidPage(plotlyOutput("trendPlot",width = "100%",height=650),
                                                            absolutePanel(h4("Geographic Distribution of..."),id = "controls", class = "panel", fixed = TRUE,draggable = TRUE,
                                                                          top =300, left = 50, right = "auto", bottom = "auto",
                                                                          width = 240, height = "auto",
                                            radioButtons("geotype", "",
                                                         c("# of Products" = "mug_country",
                                                           "# of Products, excluding USA" = "mug_country_noUSA",
                                                           "# of Collectors" = "user_country",
                                                           "# of Collectors, excluding USA" = "user_country_noUSA"),
                                                         selected="mug_country")))),
                             tabPanel("K-Means Clustering",fluidPage(plotlyOutput("kmeans",width = "100%",height=650))),
                             tabPanel("Collectible Editions",fluidPage(plotlyOutput("plotedition",width = "100%",height=650))),
                             tabPanel("Collectible Country",fluidPage(plotlyOutput("plotcountry",width = "100%",height=650)))
                             
                              )))