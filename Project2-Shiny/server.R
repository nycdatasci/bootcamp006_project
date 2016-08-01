library(shiny)
library(leaflet)
library(ggplot2)
library(reshape2)
library(googleVis)
library(dplyr)
library(plotly)

shinyServer(function(input, output) {
  
  reduced = reactive({
    Year = input$Year
    Country = input$Ctry
    Region = input$Reg
    
    filtered.data = data

    if(Country == "All" & Year == "All" & Region == "All") {
      return(data)
    } else {

      if(Year == "All")    { Year    = year.set}
      if(Country == "All") { Country = country.set}
      if(Region == "All")  { Region  = region.set}
      
      filtered.data = filtered.data %>% filter(country_txt %in% Country & iyear %in% Year & 
                                                 region_txt %in% Region)
    }
  
    })
  
  ###### END OF REACTIVE FUNCTION #############
  
  # stacked <- reactive({
  #   dat.stack = melt(reduced(), id.vars = melt.ids)
  #   dat.stack$value = as.integer(dat.stack$value)
  # })

  ###### END OF REACTIVE FUNCTION #############

  output$tree = renderGvis({
    country.freq = melt(reduced(), id.vars = melt.ids)
    country.freq = country.freq %>% filter(variable == "All" & value == 1) %>% group_by(country_txt) %>%
      summarise(count = n(), Parent = "Proportion of Terrorist Attacks across the World")

    world.freq = country.freq %>% summarise(Parent = NA, country_txt = "Proportion of Terrorist Attacks across the World", count = sum(count))
    world = rbind(country.freq, world.freq)
    world$count.log=log(world$count)
    world = mutate(world, percentage = ifelse(country_txt != "Proportion of Terrorist Attacks across the World", (count/ as.integer(world[ world$country_txt == "Proportion of Terrorist Attacks across the World", "count"])*100), NA))
    worldtree = gvisTreeMap(world, "country_txt", "Parent", "count", "percentage", 
                            options=list(width=1000, height=700, minColor = "#ECD70E", maxColor = "#DF4018", midColor = "Orange",
                                         headerHeight = 30, headerColor = "Grey"))
    
  })
 
  
  output$tree.table = renderGvis ({
    country.freq = melt(reduced(), id.vars = melt.ids)
    country.freq = country.freq %>% filter(variable == "All" & value == 1) %>% group_by(country_txt) %>%
      summarise(count = n(), Parent = "Proportion of Terrorist Attacks across the World")
    
    world.freq = country.freq %>% summarise(Parent = NA, country_txt = "Proportion of Terrorist Attacks across the World", count = sum(count))
    world = rbind(country.freq, world.freq)
    world$count.log=log(world$count)
    world = mutate(world, percentage = ifelse(country_txt != "Proportion of Terrorist Attacks across the World", (count/ as.integer(world[ world$country_txt == "Proportion of Terrorist Attacks across the World", "count"])*100), NA))
    
    world_1 = world %>% select(Country = country_txt, Percentage = percentage) %>% arrange(desc(Percentage)) %>% top_n(20)
    
    tbl1 = gvisTable(world_1)
  })
  
  output$map_leaflet = renderLeaflet({
    reduced.data = reduced()
    # m = leaflet(reduced.data) %>% addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%
    #    addCircleMarkers(~longitude, ~latitude, popup = ~summary, radius= 6, col=~pal(Marker))
    m = leaflet(reduced.data) %>% addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%
      addCircleMarkers(~longitude, ~latitude, radius= 7, popup = ~summary, color = "red")
  })
  
  
  output$trend <- renderPlot({
      Int = input$INTS
      Weapons = input$WeaponType
      AttackTypes = input$AttackType
      Targets = input$TargetTypes
      Absolute = input$AbsoluteNos
      
      if (Absolute) {
        melt(reduced(), id.vars = melt.ids) %>% group_by(iyear, variable) %>%
          filter(variable %in% Int | variable %in% Weapons | variable %in% AttackTypes | variable %in% Targets) %>%
          summarise(sum = sum(value, na.rm=T)) %>%
          ggplot(aes(x=iyear, y = sum)) + geom_line(aes(group = variable, color = variable)) +
          scale_color_brewer(palette = "Set1", name = "Series") + ggtitle("Attack Type: Trends") + theme_bw() +
          xlab("Year") + ylab("Count")
      } else {
      melt(reduced(), id.vars = melt.ids) %>% group_by(iyear, variable) %>% 
      filter(variable %in% Int | variable %in% Weapons | variable %in% AttackTypes | variable %in% Targets) %>% 
      summarise(avg = mean(value, na.rm=T)*100) %>% 
      ggplot(aes(x=iyear, y = avg)) + geom_line(aes(group = variable, color = variable)) + 
      scale_color_brewer(palette = "Set1", name = "Series") + ggtitle("Attack Type: Trends") + theme_bw() +
      xlab("Year") + ylab("Percent")
      }
  })
  
  
  output$bar <- renderPlot({
    Int = input$INTS.Bar
    
    if (input$AllWeapons) {
      Weapons = WeaponFilters
    } else {
    Weapons = input$WeaponType.Bar
    }
    
    if (input$AllAttacks) {
      AttackTypes = AttackTypeFilter
    } else {
    AttackTypes = input$AttackType.Bar
    }
    
    if (input$AllTargets) {
      Targets = TargetChoices
    } else {
    Targets = input$TargetTypes.Bar
    }
    
    
    melt(reduced(), id.vars = melt.ids) %>% group_by(variable) %>% 
      filter(variable %in% Int | variable %in% Weapons | variable %in% AttackTypes | variable %in% Targets) %>%
      summarise(avg = mean(value, na.rm=T)*100, base = n(), sum = sum(value, na.rm=T)) %>% 
      ggplot(aes(x = variable, y=avg, fill = variable)) + geom_bar(stat="identity") + 
      scale_fill_brewer(palette = "Set3", name = "Legend") + theme_bw() + theme(legend.position = "none") +
      xlab("Series") + ylab("Average Percentage") + ggtitle("Point in Time Values") +
      geom_text(aes(x = variable, y = avg+1, label = paste0(round(avg,2),"% (#: ", sum, ")"))) +
      geom_text(aes(x = variable[length(variable)], y = max(avg)+3, label = paste0("BASE SIZE: (",base,")"), color = "indianred2"))
  })
  
  
  output$comparison <- renderPlot({
    Country1 = input$C1
    Country2 = input$C2
    Country3 = input$C3
    Country4 = input$C4
    Country5 = input$C5
    Series = input$SeriesSelect
    

    melt(reduced(), id.vars = melt.ids) %>% group_by(country_txt) %>% 
      filter(variable %in% Series & 
                (country_txt %in% Country1 | country_txt %in% Country2 | country_txt %in% Country3  | country_txt %in% Country4 | 
                  country_txt %in% Country5)) %>%
      summarise(avg = mean(value, na.rm=T)*100, base = n(), sum = sum(value, na.rm=T)) %>% 
      ggplot(aes(x = country_txt, y=avg, fill = country_txt)) + geom_bar(stat="identity") + 
      scale_fill_brewer(palette = "Set3") + theme_bw() + theme(legend.position = "none") +
      xlab("") + ylab("Average Percentage") + ggtitle("Country Comparison") +
      geom_text(aes(x = country_txt, y = avg+1, label = paste0(round(avg,2),"% (#: ", sum, ")"))) +
      geom_text(aes(x = country_txt, y = max(avg)+5, label = paste0("BASE SIZE: (",base,")"), color = "indianred2"))
  })
  
  output$overall.distribution <- renderPlot({
    Annual.Change %>% ggplot(aes(x = iyear, y=occurences, fill = "blue")) + geom_bar(stat="identity") + 
      ggtitle("Number of Observations by Year") + theme_bw() + theme(legend.position = "none") +
      scale_fill_brewer(palette = "Blues") + xlab("Year") + ylab("Number of Attacks")
  })
  
  output$overall.distribution.change <- renderPlot({
    Annual.Change %>% ggplot(aes(x = iyear, y=change, fill = "blue")) + geom_bar(stat="identity", position = "identity") + 
      ggtitle("Percentage Change in Recorded Events Vs. Previous Year") + theme_bw() + theme(legend.position = "none") +
      scale_fill_brewer(palette = "Blues") + xlab("Year") + ylab("Percent Change") +
      geom_text(data = annual.change.labels, aes(x = iyear, y = change+3, label = paste0(round(annual.change.labels$change,1),"%"), 
                                                 color = "indianred2"))
  })

  
  
  
})

