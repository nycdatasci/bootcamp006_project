map_ratio <- function(var, color, legend.title){
  
  # --- create palette ------
  pal <- colorNumeric(
    palette = color,
    domain = var
  )
  
  # --- create leaflet map ----
    leaflet(worldaps) %>%
      setView(lng = -64.787342, lat = 32.300140, zoom = 2) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                  color = ~pal(var)
      ) %>%
    
  # --- create legend -------  
      addLegend("bottomleft", pal = pal, 
                values = ~var,
                title = legend.title,
                labFormat = labelFormat(suffix = "%"),
                opacity = 1
      )
  
}