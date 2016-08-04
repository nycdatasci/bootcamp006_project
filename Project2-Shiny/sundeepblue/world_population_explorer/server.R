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

function(input, output) {
    # render the global sphere
    output$global_sphere = renderGlobe({
        area = input$world_area_select_for_global_sphere
        names = get_country_names_by_area(area)
        countries_to_plot = country_locations %>% filter(country_names %in% names)
        
        # get the population of that year for the specified countries
        year = input$year_slider
        p = population_of_countries_df %>%
            select(Place, ends_with(as.character(year))) %>%
            filter(Place %in% names)
        
        # adjust bar height based on population
        bar_height = p[, 2] / max(p[, 2]) * 500
        
        background_color = ifelse(input$sphere_view_background, "black", "white")
        
        earth_surface_img = "data/img/world.topo.bathy.200412.3x5400x2700.jpg"
        globejs(
            img = earth_surface_img,
            emmisive = "#000000",
            bodycolor = "#000000",
            lightcolor = "#aaaa44",
            
            lat = countries_to_plot$lat,
            long = countries_to_plot$lon,
            value = bar_height,
            
            fov = 45,
            color = "red",
            bg = background_color,
            pointsize = 5
        )
    })
    
    # render the leaflet map with polygons
    output$leaf_map = renderLeaflet({
        # option 1: download from web
        # download.file(file.path('http://www.naturalearthdata.com/http/',
        #                         'www.naturalearthdata.com/download/50m/cultural',
        #                         'ne_50m_admin_0_countries.zip'),
        #               f <- tempfile())
        # unzip(f, exdir=tempdir())
        
        # option 2: load from locally saved map
        area = input$world_area_select_for_global_sphere
        names = get_country_names_by_area(area)
        
        leaflet() %>%
            addTiles() %>%
            addPolygons(
                data = subset(world_ogr, name %in% names),
                weight = 2,
                color = "red",
                fillColor = "green",
                fillOpacity = 0.3
            )
    })
    
    # generate and show the country name cloud image
    output$population_country_cloud = renderPlot({
        year = input$year_slider
        population_of_countries_in_one_year = population_of_countries_df %>%
            select(Place, ends_with(as.character(year)))
        
        #set.seed(4363)
        wordcloud(
            population_of_countries_in_one_year$Place,
            population_of_countries_in_one_year[, 2],
            scale = c(6, 1),
            max.words=100,
            rot.per = 0.2,
            colors = brewer.pal(8, "Dark2"),
            random.color = TRUE
        )
    })
    
    output$population_of_one_country_data_table = DT::renderDataTable({
        year = input$year_slider
        p = population_of_countries_df %>%
            select(Place, ends_with(as.character(year)))
        
        p$ratio = p[, 2] / sum(p[, 2])
        p$ratio = specify_decimal(p$ratio, 4)
        
        population_of_countries_in_one_year = p
        datatable(
            population_of_countries_in_one_year,
            selection = 'single',
            options = list(pageLength = 10, searchHighlight = TRUE)
        ) %>%
            formatStyle('Place',  color = 'red')
    })
    
    # ------------------------------------------------------------------------------------------
    # render info boxes for 5 major countries
    
    # info box for China
    output$china_info_box = renderInfoBox({
        country_name = "China"
        res = get_population_and_ratio_of_country_in_a_year(country_name, input$year_slider)
        infoBox(
            paste0(country_name, " in ", input$year_slider),
            paste0("Total: ", res[1], ", ratio: ", res[2]),
            icon = icon("cny"),
            color = "red",
            width = 2
            #href = "http://www.worldometers.info/world-population/china-population/"
        )
    })
    
    # info box for India
    output$india_info_box = renderInfoBox({
        country_name = "India"
        res = get_population_and_ratio_of_country_in_a_year(country_name, input$year_slider)
        infoBox(
            paste0(country_name, " in ", input$year_slider),
            paste0("Total: ", res[1], ", ratio: ", res[2]),
            icon = icon("inr"),
            color = "purple",
            width = 2
        )
    })
    
    # info box for USA
    output$usa_info_box = renderInfoBox({
        country_name = "United States of America"
        res = get_population_and_ratio_of_country_in_a_year(country_name, input$year_slider)
        infoBox(
            paste0(country_name, " in ", input$year_slider),
            paste0("Total: ", res[1], ", ratio: ", res[2]),
            icon = icon("usd"),
            color = "blue",
            width = 2
        )
    })
    
    # info box for Indonesia
    output$indonesia_info_box = renderInfoBox({
        country_name = "Indonesia"
        res = get_population_and_ratio_of_country_in_a_year(country_name, input$year_slider)
        infoBox(
            paste0(country_name, " in ", input$year_slider),
            paste0("Total: ", res[1], ", ratio: ", res[2]),
            icon = icon("money"),
            color = "green",
            width = 2
        )
    })
    
    # info box for Brazil
    output$brazil_info_box = renderInfoBox({
        country_name = "Brazil"
        res = get_population_and_ratio_of_country_in_a_year(country_name, input$year_slider)
        infoBox(
            paste0(country_name, " in ", input$year_slider),
            paste0("Total: ", res[1], ", ratio: ", res[2]),
            icon = icon("usd"),
            color = "lime",
            width = 2
        )
    })
    
    # info box for Pakistan
    output$pakistan_info_box = renderInfoBox({
        country_name = "Pakistan"
        res = get_population_and_ratio_of_country_in_a_year(country_name, input$year_slider)
        infoBox(
            paste0(country_name, " in ", input$year_slider),
            paste0("Total: ", res[1], ", ratio: ", res[2]),
            icon = icon("inr"),
            color = "fuchsia",
            width = 2
        )
    })
    
    
    # ------------------------------------------------------------------------------------------
    
    # render world map
    output$world_map_plot <- renderGvis({
        selected_year = input$year_slider
        population_of_countries_in_one_year = population_of_countries_df %>%
            select(Place, ends_with(as.character(selected_year)))
        
        gvisGeoChart(
            population_of_countries_in_one_year,
            locationvar = 'Place',
            colorvar = paste0("X", selected_year),
            options = list(
                width = 1000,
                height = 600,
                backgroundColor = "black",
                colors = "['blue', 'green', 'cyan', 'yellow', 'magenta', 'red']"
            )
        )
    })
    
    # render population data table
    output$population_data_table = DT::renderDataTable(
        datatable(
            population_of_countries_df,
            selection = 'single',
            options = list(pageLength = 15, searchHighlight = TRUE)
        ) %>%
            formatStyle('Place',  color = 'red') %>%
            formatStyle('Notes',  color = 'blue', fontWeight = 'bold')
    )
    
    output$world_population_scatter_graph = renderPlotly({
        selected_area = input$world_area_select
        start_and_end_index_of_areas = get_start_and_end_index_of_area(selected_area)
        
        # start to draw the curves
        start_index = start_and_end_index_of_areas[1]
        end_index = start_and_end_index_of_areas[2]
        draw_plotly_population_by_continent(start_index, end_index)
    })
    
    output$boxplot_graph = renderPlot({
        po = population_of_countries_df %>%
            select(Place, ends_with(as.character(2015)))
        
        p <- ggplot(po[1:5, ], aes(Place, X2015))
        p + geom_boxplot() + geom_jitter(width = 0.2)
    })
}
