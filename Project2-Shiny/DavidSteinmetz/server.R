suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(ggplot2))

shinyServer(function(input, output, session) {

    
# Maps on Accident tab ----------------------------------------------------
    
    # Show US accident maps using googleVis
    output$us_map <- renderGvis({
        gvisGeoChart(
            acc[, .N, by = 'DEF'],
            "DEF",
            'N',
            options = list(
                region = "US",
                displayMode = "regions",
                resolution = "provinces",
                width = "auto",
                height = "auto",
                colorAxis = "{colors:['green', 'yellow', 'orange', 'red']}"
            )
        )
    })
    
    # Calculate per capita accident rates
    n.pop <- merge(acc[,.N,by=DEF], pop, by.x='DEF', by.y='STATE')
    n.pop[,N.POP:=round((N/POP_2014)*1e6,0)]
    
    # Per capita accident map
    output$us_map_bypop <- renderGvis({
        gvisGeoChart(
            n.pop,
            "DEF",
            'N.POP',
            options = list(
                region = "US",
                displayMode = "regions",
                resolution = "provinces",
                width = "auto",
                height = "auto",
                colorAxis = "{colors:['green', 'yellow', 'orange', 'red']}"
            )
        )
    })

    

# Maps on transportation tab ----------------------------------------------

    job_sums <- jobs[, .(tot_emp=sum(tot_emp, na.rm = TRUE)), by = area_title]
    
    # Transportation jobs map
    output$trans_map <- renderGvis({
        gvisGeoChart(
            job_sums,
            "area_title",
            'tot_emp',
            options = list(
                region = "US",
                displayMode = "regions",
                resolution = "provinces",
                width = "auto",
                height = "auto",
                colorAxis = "{colors:['blue']}"
            )
        )
    })

    # Calculate per capita accident rates
    n.jobs <- merge(job_sums, pop, by.x='area_title', by.y='STATE')
    n.jobs[,PER.100:=round((tot_emp/POP_2014)*100,2)]
    
    # Per capita accident map
    output$trans_map_bypop <- renderGvis({
        gvisGeoChart(
            n.jobs,
            "area_title",
            'PER.100',
            options = list(
                region = "US",
                displayMode = "regions",
                resolution = "provinces",
                width = "auto",
                height = "auto",
                colorAxis = "{colors:['blue']}"
            )
        )
    })
    
    
# Leaflet map -------------------------------------------------------------

    # create interactive map
    output$map_lf <- renderLeaflet({
        leaflet() %>%
        addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
        setView(lng = -95,
            lat = 40,
            zoom = 4) %>%
        addMarkers(
            data = acc,
            lat = ~ LATITUDE,
            lng = ~ LONGITUD,
            clusterOptions = markerClusterOptions(disableClusteringAtZoom=11))
    })
    

    
# Data table page ---------------------------------------------------------
    
    # show data using DataTable
    output$table <- DT::renderDataTable({
        datatable(acc, rownames = FALSE) %>%
            formatStyle(input$selected,
                        background = "skyblue",
                        fontWeight = 'bold')
    })
    

# Floating panel on interactive map ---------------------------------------
    
    # Select visible accidents on map    
    acc_on_map <- reactive({
        if (is.null(input$map_lf_bounds))
            return(acc[FALSE,])
        bounds <- input$map_lf_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        acc[LATITUDE >= latRng[1] & LATITUDE <= latRng[2] &
                LONGITUD >= lngRng[1] & LONGITUD <= lngRng[2]]
    })
    
    # Hour bar chart in floating panel of interactive map
    output$bar_hr <- renderPlot({
        g <- ggplot(data=acc_on_map()[HOUR!=99,.N,by=HOUR], 
                    aes(x=factor(HOUR),y=N))
        g <- g + geom_bar(stat='identity')
        g <- g + xlab('Hour of the Day')
        g <- g + ylab('Accidents')
        g <- g + theme_minimal()
        g
    })

    # Month bar chart in floating panel of interactive map
    output$bar_mn <- renderPlot({
        g <- ggplot(data=acc_on_map()[,.N,by=MONTH], 
                    aes(x=factor(MONTH),y=N))
        g <- g + geom_bar(stat='identity')
        g <- g + xlab('Month')
        g <- g + ylab('Accidents')
        g <- g + theme_minimal()
        g
    })

    # Day bar chart in floating panel of interactive map
    output$bar_dy <- renderPlot({
        g <- ggplot(data=acc_on_map()[,.N,by=DAY_WEEK], 
                    aes(x=factor(DAY_WEEK),y=N))
        g <- g + geom_bar(stat='identity')
        g <- g + xlab('Day of the Week')
        g <- g + ylab('Accidents')
        g <- g + theme_minimal()
        g
    })
    
    # Text at bottom of floating panel of interactive map
    output$tot <- renderText({paste('Accidents on map:',acc_on_map()[,.N])})
    
})