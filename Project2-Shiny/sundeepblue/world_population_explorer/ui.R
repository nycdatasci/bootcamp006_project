######################################################
# by Chuan Sun (sundeepblue at gmail dot com)
# https://twitter.com/sundeepblue
# 7/31/2016
# Shiny project @ NYC Data Science Academy
######################################################

library(googleVis)
library(shiny)
library(DT)
library(plotly)
library(shinydashboard)
library(XML)
library(threejs)
library(maps)
library(leaflet)
library(dplyr)
library(rgdal)
library(wordcloud)


# read csv loading by reading from a R source file
source("helper.R")

dashboardPage(
    skin = "purple",
    
    dashboardHeader(
        title = "World Population Explorer V0.1",
        disable = FALSE,
        titleWidth = 400
    ),
    
    dashboardSidebar(
        width = 250,
        
        sidebarMenu(
            menuItem("Visualization",
                     tabName = "Visualization",
                     icon = icon("th")),
            menuItem(
                "Resources",
                tabName = "Resources",
                icon = icon("book", lib = "glyphicon")
            ),
            menuItem("About",
                     tabName = "About",
                     icon = icon("dashboard")),
            
            sliderInput(
                "year_slider",
                width = "100%",
                label = h3("Select year:"),
                min = 1950,
                max = 2015,
                value = 2015,
                ticks = TRUE,
                animate = FALSE
            )
        )
    ),
    
    dashboardBody(
        tags$head(tags$style(
            HTML(
                '
                .main-header
                .logo {
                font-family: "Georgia", Times, "Times New Roman", serif;
                font-weight: bold;
                font-size: 20px;
                }
                '
            )
            )),
        
        tabItems(
            tabItem(
                tabName = "Visualization",
                tabsetPanel(
                    type = "tabs",
                    tabPanel(
                        "Global Population by Continent",
                        p(""),
                        p("Select continent from dropdown menu. In global sphere view, the red vertical bars indicate the population of a country/area in that continent. "),
                        p("Accordingly, in global map view, all countries of that continent will appear on map."),
                        selectInput(
                            inputId = "world_area_select_for_global_sphere",
                            label = "Choose continent:",
                            width = "30%",
                            choices = world_continent_list,
                            selected = "World level overview"
                        ),
                        checkboxInput("sphere_view_background", label = "black background for sphere", value = TRUE),
                        
                        fluidRow(
                            title = "Global view",
                            box(
                                title = "Global Sphere View",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                globeOutput("global_sphere", width =
                                                "100%", height = 600)
                            ),
                            box(title = "Global Map View",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                leafletOutput(
                                    "leaf_map", width = "100%", height = 600
                                ))
                        )
                    ),

                    tabPanel(
                        "Population by Country",
                        p(""),
                        p("Population is in thousand. Hover your mouse on the map, and the country name and its population will appear."),
                        htmlOutput("world_map_plot", width =
                                       "100%", height = 600)
                    ),
                    
                    tabPanel(
                        "Global Population by Year",
                        p(""),
                        p(),
                        fluidRow(
                            title = "Population cuntry cloud",
                            box(
                                title = "Population country cloud",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotOutput(
                                    "population_country_cloud",
                                    width = "100%",
                                    height = 500
                                )
                            ),
                            box(
                                title = "Population data table in one year",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                DT::dataTableOutput(
                                    "population_of_one_country_data_table",
                                    width = "100%",
                                    height = 500
                                )
                            )
                        ),
                        
                        h2("Top 6 countries by population"),
                        fluidRow(
                            title = "Top 6 countries by population",
                            infoBoxOutput("china_info_box"),
                            infoBoxOutput("india_info_box"),
                            infoBoxOutput("usa_info_box"),
                            infoBoxOutput("indonesia_info_box"),
                            infoBoxOutput("brazil_info_box"),
                            infoBoxOutput("pakistan_info_box")
                        )
                    ),
                    
                    tabPanel(
                        "Global Population by Area",
                        h3("Population by area from 1950 to 2015:"),
                        selectInput(
                            inputId = "world_area_select",
                            label = "Choose area:",
                            width = "50%",
                            choices = world_area_list,
                            selected = "World level overview"
                        ),
                        plotlyOutput(
                            "world_population_scatter_graph",
                            width = "100%",
                            height = 500
                        )
                        #htmlOutput("population_column_chart")
                    ),
                    
                    tabPanel(
                        "Population Data Table",
                        p(""),
                        DT::dataTableOutput("population_data_table"),
                        h1("Notes:"),
                        p("The dataset was downloaded from the Population Division of United Nations: https://esa.un.org/unpd/wpp/Download/Standard/Population/"),
                        p(
                            "(a) More developed regions comprise Europe, Northern America, Australia/New Zealand and Japan."
                        ),
                        p(
                            "(b) Less developed regions comprise all regions of Africa, Asia (except Japan), Latin America and the Caribbean plus Melanesia, Micronesia and Polynesia."
                        ),
                        p(
                            "(c) The group of least developed countries, as defined by the United Nations General Assembly in its resolutions (59/209, 59/210, 60/33, 62/97, 64/L.55, 67/L.43, 64/295) included 48 countries in January 2014:  34 in Africa, 9 in Asia, 4 in Oceania and one in Latin America and the Caribbean."
                        ),
                        p(
                            "(d) Other less developed countries comprise the less developed regions excluding the least developed countries."
                        ),
                        p(
                            "(e) The country classification by income level is based on 2014 GNI per capita from the World Bank."
                        ),
                        p(
                            "(f) Sub-Saharan Africa refers to all of Africa except Northern Africa."
                        ),
                        p("(1) Including Agalega, Rodrigues and Saint Brandon."),
                        p("(2) Including Zanzibar."),
                        p("(3) Including Ascension and Tristan da Cunha."),
                        p(
                            "(4) For statistical purposes, the data for China do not include Hong Kong and Macao, Special Administrative Regions (SAR) of China, and Taiwan Province of China."
                        ),
                        p(
                            "(5) As of 1 July 1997, Hong Kong became a Special Administrative Region (SAR) of China."
                        ),
                        p(
                            "(6) As of 20 December 1999, Macao became a Special Administrative Region (SAR) of China."
                        ),
                        p(
                            "(7) The regions Southern Asia and Central Asia are combined into South-Central Asia."
                        ),
                        p("(8) Including Sabah and Sarawak."),
                        p("(9) Including Nagorno-Karabakh."),
                        p("(10) Refers to the whole country"),
                        p("(11) Including Abkhazia and South Ossetia."),
                        p("(12) Including East Jerusalem."),
                        p("(13) Including Transnistria."),
                        p("(14) Including Crimea"),
                        p("(15) Refers to Guernsey, and Jersey."),
                        p("(16) Including Åland Islands."),
                        p("(17) Including Svalbard and Jan Mayen Islands."),
                        p("(18) Refers to the Vatican City State."),
                        p("(19) Including Kosovo."),
                        p("(20) Including Canary Islands, Ceuta and Melilla."),
                        p("(21) The former Yugoslav Republic of Macedonia."),
                        p("(22) Refers to Bonaire, Saba and Sint Eustatius."),
                        p(
                            "(23) Including Saint-Barthélemy and Saint-Martin (French part)."
                        ),
                        p(
                            "(24) Including Christmas Island, Cocos (Keeling) Islands and Norfolk Island."
                        ),
                        p("(25) Including Pitcairn.")
                    )
                )
            ),
            
            tabItem(tabName = "Resources",
                    fluidRow(
                        box(
                            title = "World Population History: From 1 to 2050 AD",
                            status = "primary",
                            solidHeader = FALSE,
                            collapsible = TRUE,
                            HTML(
                                '<iframe width="100%" height="300" src="https://www.youtube.com/embed/SObnO6jnAD8" frameborder="0" allowfullscreen></iframe>'
                            )
                        ),
                        box(
                            title = "Examination of the World's Population with Joel Cohen",
                            status = "primary",
                            solidHeader = FALSE,
                            collapsible = TRUE,
                            HTML(
                                '<iframe width="100%" height="300" src="https://www.youtube.com/embed/PZrmYp4USWo" frameborder="0" allowfullscreen></iframe>'
                            )
                        ),
                        box(
                            title = "Overpopulation - The Future of Planet Earth - Documentary",
                            status = "primary",
                            solidHeader = FALSE,
                            collapsible = TRUE,
                            
                            HTML(
                                '<iframe width="100%" height="300" src="https://www.youtube.com/embed/Hn5sEipg5tc" frameborder="0" allowfullscreen></iframe>'
                            )
                        ),
                        box(
                            title = "7 Billion: How Did We Get So Big So Fast?",
                            status = "primary",
                            solidHeader = FALSE,
                            collapsible = TRUE,
                            HTML(
                                '<iframe width="100%" height="300" src="https://www.youtube.com/embed/VcSX4ytEfcE" frameborder="0" allowfullscreen></iframe>'
                            )
                        )
                    )),
            
            tabItem(
                tabName = "About",
                p("Author: Chuan Sun (sundeepblue at gmail dot com)"),
                p("https://twitter.com/sundeepblue"),
                p("7/31/2016"),
                p("Shiny project @ NYC Data Science Academy")
            )
        )
    )
)
