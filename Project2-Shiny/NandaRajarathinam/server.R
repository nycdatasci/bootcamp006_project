library(googleVis)
library(shiny)

shinyServer(function(input, output) {
  
  ### Code for Insight: volume of complaints tab
  
  #choiceResponseInsight <- reactive({
  #  capitalize(input$responseInsightRB)
  #})
  #output$txtResponseInsight <- renderText({
  #  if (choiceResponseInsight() == "Volume of Complaints")    {  "Volume of Complaints" }
  #  if  (choiceResponseInsight() == "% Disputed Responses")   {  "Percentage of Disputed Responses"    }
  #  if  (choiceResponseInsight() == "% Late Responses")   { "Percentage of Late Responses"  }
  #  if  (choiceResponseInsight() == "% Cases closed with Monetary Relief")   { "Percentage of Cases closed with Monetary Relief"  }
  #  print (choiceResponseInsight())
  # })
  
  
  graphInput <- reactive({
    # Choosing information based on widget inputs
    print (input$productCG)
    
    #if (input$responseInsightRB == "% Disputed Responses") { graphdat <- reshaped_disputed_response }
    #if (input$responseInsightRB == "% Late Responses") { graphdat <- reshaped_late_response }
    #if (input$responseInsightRB == "Volume of Complaints") { graphdat <- consumer_compl_by_prod_year }
    #if (input$responseInsightRB == "% Cases closed with Monetary Relief") { graphdat <- reshaped_monetary_relief }
    
    graphdat <- consumer_compl_by_prod_year
   
    i = c()
    if ("Bank account or service" %in% input$productCG) {
      i[length(i)+1] = 2
    }
    if ("Consumer Loan" %in% input$productCG) {
      i[length(i)+1] = 3
    }
    if ("Credit card" %in% input$productCG) {
      i[length(i)+1] = 4
    }
    if ("Credit reporting" %in% input$productCG) {
      i[length(i)+1] = 5
    }
    if ("Debt collection" %in% input$productCG) {
      i[length(i)+1] = 6
    }
    if ("Money transfers" %in% input$productCG) {
      i[length(i)+1] = 7
    }
    if ("Mortgage" %in% input$productCG) {
      i[length(i)+1] = 8
    }
    if ("Student loan" %in% input$productCG) {
      i[length(i)+1] = 9
    }
    firstcol = 1
    print(i)
    i = c(firstcol, i)
     print(i)
    graphdat = graphdat[, c(i)]
    
     return (data.frame(graphdat))
  
  })
  
  
  # Graphing data of interest
  output$plot <- renderGvis({
    print (graphInput())
    
    gvisLineChart(
      graphInput(), options=list(
        lineWidth=3, pointSize=5,
        vAxis="{title:'Number of Complaints'  }",
        hAxis="{title:'Year', format:'#####'}", 
        width=750, height=500))
    
  })
  

  ### Code for Insight: Responses tab
  
  choiceResponseInsight <- reactive({
    capitalize(input$responseInsightRB)
  })
  output$txtResponseInsight <- renderText({
    if  (choiceResponseInsight() == "% Disputed Responses")   {  "Percentage of Disputed Responses"    }
    if  (choiceResponseInsight() == "% Delayed Responses")   { "Percentage of Delayed Responses"  }
    if  (choiceResponseInsight() == "% Cases closed with Monetary Relief")   { "Percentage of Cases closed with Monetary Relief"  }
    print (choiceResponseInsight())
  })
  
  
  graphInputInsight <- reactive({
    # Choosing information based on widget inputs
    print (input$productInsightCG)
    
    if (input$responseInsightRB == "% Disputed Responses") { graphdat <- reshaped_disputed_response }
    if (input$responseInsightRB == "% Delayed Responses") { graphdat <- reshaped_late_response }
    if (input$responseInsightRB == "% Cases closed with Monetary Relief") { graphdat <- reshaped_monetary_relief }
    
    #graphdat <- consumer_compl_by_prod_year
    
    i = c()
    if ("Bank account or service" %in% input$productInsightCG) {
      i[length(i)+1] = 2
    }
    if ("Consumer Loan" %in% input$productInsightCG) {
      i[length(i)+1] = 3
    }
    if ("Credit card" %in% input$productInsightCG) {
      i[length(i)+1] = 4
    }
    if ("Credit reporting" %in% input$productInsightCG) {
      i[length(i)+1] = 5
    }
    if ("Debt collection" %in% input$productInsightCG) {
      i[length(i)+1] = 6
    }
    if ("Money transfers" %in% input$productInsightCG) {
      i[length(i)+1] = 7
    }
    if ("Mortgage" %in% input$productInsightCG) {
      i[length(i)+1] = 8
    }
    if ("Student loan" %in% input$productInsightCG) {
      i[length(i)+1] = 9
    }
    firstcol = 1
    print(i)
    i = c(firstcol, i)
    print(i)
    graphdat = graphdat[, c(i)]
    
    return (data.frame(graphdat))
    
  })
  
  
  # Graphing data of interest
  output$plotInsight <- renderGvis({
    #print (graphInput())
    
    gvisLineChart(
      graphInputInsight(), options=list(
        lineWidth=3, pointSize=5,
        vAxis="{title:'Response Metric' , format:'#.##%'  }",
        hAxis="{title:'Year', format:'#####'}", 
        width=750, height=500))
    
  })
  
  
  

  
  
  ### Code for Insight: Top Issues tab
  
  
  # Displaying data of interest
  choice <- reactive({
    capitalize(input$prodSI)
  })
  output$txtTopIssue <- renderText({
    paste( "Top 5 Issues for", choice() )
  })
  
  graphInputTopIssue <- reactive({
    # Choosing information based on widget inputs
    graphdatTopIssue <- consumer_compl_by_prod_issue
    graphdatTopIssue <- graphdatTopIssue  %>% filter(Product == input$prodSI & Year == input$yearSI)
    
    graphdatTopIssue <- graphdatTopIssue[,c(5,4)]
    graphdatTopIssue = graphdatTopIssue[order(-graphdatTopIssue$cnt_complaints), ]
    data.frame(graphdatTopIssue)
    
    
 
    
  })
  
  # Graphing data of interest
  output$plotTopIssue <- renderGvis({
    print (graphInputTopIssue())
    gvisColumnChart(
      graphInputTopIssue(), xvar="Issue_clean", yvar="cnt_complaints" ,options=list(
        lineWidth=3, pointSize=5, 
        vAxis="{title:'Number of Complaints'}",
        hAxis="{title:'Issue'}", 
        width=950, height=500))
  })
  
  
  ### Code for Insight: Companies tab
  # Displaying data of interest
  choiceResponse <- reactive({
    capitalize(input$responseRB)
  })
  output$txtResponseMetric <- renderText({
     if (choiceResponse() == "% Disputed Responses")
     {  "Percentage of Disputed Responses" }
     else {  "Percentage of Delayed Responses"    }
  })
  
  graphInputResponseMetric <- reactive({
    # Choosing information based on widget inputs
    if (input$responseRB == "% Disputed Responses") { graphdatResponseMetric <- top25_institution_data_merged_disp_desc }
    if (input$responseRB == "% Delayed Responses") { graphdatResponseMetric <- top25_institution_data_merged_sla_desc }
    #graphdatResponseMetric <- top25_institution_data_merged_disp_desc
    graphdatResponseMetric <- graphdatResponseMetric  %>% filter(Product == input$prodResponseMetricSI & Tier == input$tierResponseMetricSI & Year == input$yearResponseMetricSI)
    

    
    #if (input$responseRB == "% Late Responses") {
    #  graphdatResponseMetric <- graphdatResponseMetric[,c(4,8)] 
     # names(graphdatResponseMetric) = c("Company","Response Metric")
      #x_var = "Company"
      #y_var = "missed_sla_ratio"
    #}
    
    graphdatResponseMetric <- graphdatResponseMetric[,c(4,8)] 
    names(graphdatResponseMetric) = c("Company","Response.Metric")
    print (graphdatResponseMetric)
    
    graphdatResponseMetric = graphdatResponseMetric[order(-graphdatResponseMetric$Response.Metric), ]
    
    data.frame(graphdatResponseMetric)
  })
  
  # Graphing data of interest
  output$plotResponseMetric <- renderGvis({
    print (graphInputResponseMetric())
    gvisColumnChart(
      graphInputResponseMetric(), xvar="Company", yvar="Response.Metric",
      options=list(
        lineWidth=3, pointSize=5, vAxis.textPosition = "none",
        vAxis="{title:'Response.Metric', format:'#.##%' }",
      # vAxis="{title: 'Response Metric %', format:'###%' }",

        hAxis="{title:'Firm / Company'}", 
        width=1000, height=500))
  })
  
  
  ### Code for Insight: Map tab
  # Displaying data of interest
  choiceResponseMap <- reactive({
    capitalize(input$responseMapRB)
  })
  output$txtMap <- renderText({
    if (choiceResponseMap() == "Volume of Complaints")    {  "Volume of Complaints" }
    if  (choiceResponseMap() == "% Disputed Responses")   {  "Percentage of Disputed Responses"    }
    if  (choiceResponseMap() == "% Delayed Responses")   { "Percentage of Delayed Responses"  }
    print (choiceResponseMap())
  })
  
  graphInputMap <- reactive({
    # Choosing information based on widget inputs
    if (input$responseMapRB == "% Disputed Responses") { graphdatMap <- consumer_disp_map_merged }
    if (input$responseMapRB == "% Delayed Responses") { graphdatMap <- consumer_compl_map_merged_sla }
    if (input$responseMapRB == "Volume of Complaints") { graphdatMap <- consumer_disp_map_merged }
    #graphdatResponseMetric <- top25_institution_data_merged_disp_desc
    graphdatMap <- graphdatMap  %>% filter(Product == input$prodMapSI  & Year == input$yearMapSI)
    
    if (input$responseMapRB == "% Disputed Responses") {
      graphdatMap <- graphdatMap[,c(3,8)] } # dispute ratio 
    if (input$responseMapRB == "Volume of Complaints") {
      graphdatMap <- graphdatMap[,c(3,4)]}  # Volume  of Complaints 
    if (input$responseMapRB == "% Delayed Responses") {
      graphdatMap <- graphdatMap[,c(3,7)]}  # Late Responses 
    names(graphdatMap) = c("State","Metric")
    
    data.frame(graphdatMap)
  })
  
  # Graphing data of interest
  output$plotMap <- renderGvis({
    print (graphInputMap())
    gvisGeoChart(
      graphInputMap(), locationvar="State", colorvar="Metric" ,options=list(
      ##graphInputMap()    ,options=list(  
        region="US", displayMode="regions", 
        resolution="provinces",
        width=900, height=650,  colorAxis="{colors:['#deebf7', '#3182bd']}" ))
  })
  
  
  
  
  
  

})