library(googleVis)
shinyServer(function(input, output, session) {
  
  #### Food Data Overview ####
  map_reactive <- reactive(
    gvisGeoChart(data = country_stat,
                 locationvar = 'Country', colorvar = 'Count',
                 options=list(
                   region = (region_geo %>%
                               filter(Region == input$Map_RegionSelection) %>%
                               select(GeoCode))[[1]],
                   width = 'auto', height = 'auto',
                   colorAxis = color_axis
                 )
    )
  )
  
  output$map_map <- renderGvis(
    map_reactive()
  )
  
  output$map_country <- renderDataTable(
    country_stat, options = list(dom = 't')
  )
  
  output$countryBox <- renderValueBox(
    valueBox(
      value = dim(country_stat)[1],
      subtitle = "Countries Available",
      icon = icon("list"),
      color = "purple"
    )
  )
  
  #################################################################
  
  #### Food Browsing ####
  
  # Data with selected variables
  foodFilterDT <- reactive (
    food[, c('Product_name',
             'Url', 'Code',
             input$plan_CheckGroup,
             input$plan_RegionSelection),
         with = F] %>%
      filter(complete.cases(.)) %>%
      filter(.[,input$plan_RegionSelection] > 0)
  )
  
  # Based on filter values, generate index for food Data Table filtering 
  inRangeIndex <- reactive({
    # Values of slider filters passed from UI 
    filterMin <- sapply(input$plan_CheckGroup, function(s) {input[[paste0('plan_', s)]][1]})
    
    filterMax <- sapply(input$plan_CheckGroup, function(s) {input[[paste0('plan_', s)]][2]})
    
    # If no filtering, return all rows
    if (is.null(input$plan_CheckGroup)) {
      rep(TRUE, dim(foodFilterDT())[1])
    }
    # Else, return rows that within the filtering range
    else {
      rowSums(
        sapply(input$plan_CheckGroup, # For each nutrition slider used
               function(s) {foodFilterDT()[, (get(s) >= filterMin[s]) &
                                             (get(s) <= filterMax[s]) &
                                             (get(input$plan_RegionSelection) > 0)]
               })
      ) == length(input$plan_CheckGroup)
    }
  })
  
  # Food Table for display
  displayDT <- reactive(
    # Filter datatable with filters
    foodFilterDT()[inRangeIndex(), # Filter index
                   c('Product_name', 'Code',
                     input$plan_CheckGroup), # Slider selection
                   with = F] %>%
      # Add Link buttons
      mutate(link = createLink(foodFilterDT()[inRangeIndex(), Url])) %>%
      # Truncate Names for display
      setNames(c('Name', 'Code',
                 gsub('_100g', '', input$plan_CheckGroup),
                 'Link'))
  )
  
  # 
  plot <- reactive({
    Xnutri <- input$map_plotX
    Ynutri <- input$map_plotY
    
    plotdata <- food_reactive()[inRangeIndex(), 
                               c(Xnutri, Ynutri, input$plan_RegionSelection),
                               with = F] %>%
      filter(.[,input$plan_RegionSelection] > 0)
    
    ggplot(data = plotdata, aes(x = get(Xnutri), y = get(Ynutri))) +
      geom_point(color = '#2980B9', alpha = 0.5) +
      labs(x = Xnutri, y = Ynutri)
  })
  
  # output$plot <- renderText(plot())
  output$plot <- renderPlot(plot())
    
  
  ## Browsing Outputs ##
  
  output$plan_statVB <- renderUI(
    fluidRow(
      valueBox(
        value = dim(displayDT()),
        subtitle = 'Items Matching Conditions',
        width = 3),
      valueBox(
        value = length(input$plan_CheckGroup),
        subtitle = 'Filters Selected',
        width = 3),
      valueBox(
        value = dim(summaryDT()),
        subtitle = 'Items in Summary',
        width = 3),
      column(3, align = 'right',
      actionButton(
        inputId = 'plan_add_selection',
        label = 'Add Selections to Summary',
        icon("star"),
        style = "color: #fff;
                   background-color: #337ab7;
                   border-color: #2e6da4;
                   height: 100px"))
  ))
  
  output$plan_displayDT <- renderDataTable(
    displayDT(), 
    escape = F,
    options = list(autowidth = T,
                   scrollX=TRUE,
                   columnDefs = list(
                     list(width = '10px',
                          className = 'dt-center',
                          targets = -1)
                   )
    ))

  
  #################################################################
  
  #### Summary ####
  
  # Dynamic Summary Id
  summary_Url <- reactiveValues(Url = NULL)
  
  # Get id of selected products in display Data Table
  display_selected_Url <- reactive({
    # Get selections in display DT
    display_selected <- sort(as.numeric(input$plan_displayDT_rows_selected))
    # Use Url of selections as id
    foodFilterDT()[inRangeIndex(), Url][display_selected]
  })
  
  # Click button to update Summary Id
  observeEvent(input$plan_add_selection, {
    summary_Url$Url <- c(summary_Url$Url, display_selected_Url())
  })
  
  # Click button to reset summary Ids
  observeEvent(input$summary_reset, {
    summary_Url$Url <- NULL
  })
  
  # Subset foodfilterDT with current + new selected id
  summaryDT <- reactive({
    food_reactive()[Url %in% summary_Url$Url, c('Product_name', 'Code', nutritions), with = F] %>%
      setNames(c('Name', 'Code', gsub('_100g', '', nutritions)))
  })
  
  # Selected items vs. FDA suggested DV
  summaryAvgDT <- reactive({
    avg <- colMeans(summaryDT()[, -c('Name', 'Code'), with = F], na.rm = T)
    avgDT <- as.data.table(avg, keep.rownames = T) %>% rename(Nutrition = rn)
    summaryDT <- merge(avgDT, daily_nutritions, by = 'Nutrition') %>%
      mutate('%' = round(avg / DV, 2))
  })
  
  summaryValues <- reactive(summaryAvgDT()[, get('%')])
  summaryItems <- reactive(summaryAvgDT()$Nutrition)
  
  ## Outputs ##
  
  # Dynamic ValueBox for summary view
  output$summary_infoUI <- renderUI(
    box(status = 'primary', solidHeader = T,
        title = 'Selection vs. DV - Overview', width = 12,
    fluidRow(
      lapply(1:8, function(i) {
        
        value <- ifelse(is.na(summaryValues()[i]),
                        0,
                        round(summaryValues()[i] * 100, 1)
                        )
        
        valueBox(
          value = paste0(value, '%'),
          subtitle = summaryItems()[i],
          icon = icon(ifelse(value > 50, 'exclamation-circle', "check-circle")),
          width = 3,
          color = ifelse(value > 50, 'yellow', 'green')
        )})
    )))
  
  # stat <- renderText(dim(summaryDT())[1])
  output$summary_statVB <- renderText(
    paste0(dim(summaryDT())[1], ' Food In Summary'))
  
  # Avg vs DV Table
  output$summary_avgDT <- 
    renderDataTable(
      summaryAvgDT(),
      options = list(dom = 't')
    )
  
  # Detail Table
  output$summary_DT <- renderDataTable(
    summaryDT(),
    escape = F,
    options = list(autowidth = T,
                   scrollX=TRUE,
                   columnDefs = list(
                     list(width = '10px',
                          className = 'dt-center',
                          targets = -1)
                   )
    )
  )
  
  
})



