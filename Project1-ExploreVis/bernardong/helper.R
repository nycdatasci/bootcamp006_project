# the 2-letter state converter function
# 'x' is the column of a data.frame that holds 2 digit state codes
stateFromLower <- function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
}

#------------------------------------------------------------------------------------------------------
percent_map <- function(var, color, legend.title, min=0, max=100) {
  
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100,
                             include.lowest = TRUE,
                             ordered = TRUE))
  fills <- shades[percents]
  
  # plot choropleth map
  map("county", fill = TRUE, col = fills, resolution = 0,
      lty = 0, projection = "polyconic",
      myborder = 0, mar = c(0,0,0,0))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
      lty = 1, lwd = 1, projection = "polyconic",
      myborder = 0, mar = c(0,0,0,0))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " %"),
                   paste0(min + inc, " %"),
                   paste0(min + 2 * inc, " %"),
                   paste0(min + 3 * inc, " %"),
                   paste0(max, " %"))
  legend("bottomleft",
         legend = legend.text,
         fill = shades[c(1, 25, 50, 75, 100)],
         title = legend.title)
}

