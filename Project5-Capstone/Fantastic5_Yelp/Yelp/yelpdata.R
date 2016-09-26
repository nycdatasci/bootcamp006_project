library(rCharts)
library(devtools)
library(stringr)
library(leaflet)
library(dplyr)
library(htmltools)
library(reshape2)
library(googleVis)
library(data.table)
library(dplyr)
library(ggvis)

yelpBusiness= read.csv("./www/Biz_final_noReview_09132016.csv")
yelpBusiness =   yelpBusiness[, -1 ]
yelpBusiness = yelpBusiness %>%
  mutate(NewRating = ifelse(stars >3.5, "Good","bad"))
                              



palColor = colorFactor(c("blue", "orange"), 
                       domain = c("Good", "bad"))


YelpIcons <- icons(
  iconUrl = ifelse(yelpBusiness$stars > 3.5,
                   "http://chart.apis.google.com/chart?chst=d_map_pin_letter&chld=%E2%80%A2|abcdef&chf=a,s,ee00FFFF",
                   "http://i.stack.imgur.com/x0X0l.png"
  ),
  iconWidth = 10, iconHeight = 10,
  iconAnchorX = 10, iconAnchorY = 10
)


yelpBusiness = yelpBusiness %>%
  mutate(IconType = ifelse(stars > 3.5, "blueIcon","redIcon"))


leaflet(yelpBusiness) %>%
  addTiles() %>%  
  addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
           attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
  addMarkers(icon = YelpIcons,
  # addCircleMarkers(
  #   radius = 6, 
  #   stroke = FALSE, 
  #   fillOpacity  =  1,
  #   fillColor =  ~ palColor(yelpBusiness$NewRating),
    # clusterOptions = markerClusterOptions(disableClusteringAtZoom = 15,
    #                                       # animateAddingMarkers= TRUE,
    #                                       animate = TRUE),
popup = ~ paste( sep = "<br/>",
                 "<b>Description</b>",
                 paste('<b>business_id : </b>', business_id,sep = ' '  ),
                 paste('<b>stars :</b>', stars, sep = ' '),
                 paste('<b>AvgUserStar:</b>',  AvgUserStar, sep = ' '),
                 paste('<b>AvgReviewCount :</b>',  AvgReviewCount, sep = ' '),
                 paste('<b>AvgFriends :</b>', AvgFriends , sep = ' '),
                 paste('<b>is_Elite :</b>',  is_Elite,
                       sep = ' '),
                 paste('<b>Price:</b>',  attributes.Price.Range, sep = ' '),
                 paste('<b>AvgReviewCount :</b>',  AvgReviewCount, sep = ' '),
                 paste('<b>zip :</b>',  zip, sep = ' ')
)) %>%
setView(lng = -112,
        lat = 33.5,
        zoom = 12)   %>%
addLegend("topleft", pal = palColor,
          values = ~ yelpBusiness$NewRating,
          title = "Good and Bad Restaurants",
          opacity = 0.5   )

# m  





#####################################################################################################


##### This two libraries are necessary
library(servr)
library(LDAvis)
library(rjson)


serVis(json, out.dir = 'vis2', open.browser = T)

###################################################################################333333

blueIcon <- makeIcon(
  iconUrl = "http://chart.apis.google.com/chart?chst=d_map_pin_letter&chld=%E2%80%A2|abcdef&chf=a,s,ee00FFFF",
  iconWidth = 10, iconHeight = 10,
  iconAnchorX = 10, iconAnchorY = 10
)
redIcon <- makeIcon(
  iconUrl = "http://i.stack.imgur.com/x0X0l.png",
  iconWidth = 10, iconHeight = 10,
  iconAnchorX = 10, iconAnchorY = 10
)


leaflet(data = quakes[1:20,]) %>% addTiles() %>%
  addMarkers(~long, ~lat, ~as.character(mag), icon = redIcon)


data1 = mutate(quakes, group = cut(mag, breaks = c(0, 5, 6, Inf), labels = c("blue", "green", "orange"))) 

### I edit this png file and created my own marker.
### https://raw.githubusercontent.com/lvoogdt/Leaflet.awesome-markers/master/dist/images/markers-soft.png
quakeIcons <- iconList(blue = makeIcon("/Users/jazzurro/Documents/Stack Overflow/blue.png", iconWidth = 24, iconHeight =32),
                       green = makeIcon("/Users/jazzurro/Documents/Stack Overflow/green.png", iconWidth = 24, iconHeight =32),
                       orange = makeIcon("/Users/jazzurro/Documents/Stack Overflow/orange.png", iconWidth = 24, iconHeight =32))


leaflet(data = data1[1:100,]) %>% 
  addTiles() %>%
  addMarkers(icon = ~quakeIcons[group])



###############################################################################################3333333

#timeseries

checkingTime= read.csv("./www/checkingtime.csv")
names(checkingTime)[3:4] <- c("days","hours")

checkingtime_days = checkingTime %>%
                    group_by(days) %>%
                    summarise(checkingTotal = sum(check_in_number))
checkingtime_days['dayindex'] = c(6,2,7,1,5,3,4)
checkingtime_days = arrange(checkingtime_days, dayindex)

gdays =gvisColumnChart(data = checkingtime_days,
                   xvar = "days",
                   yvar = "checkingTotal",
                   options=list(
                     chartArea=  "{left:110,top:50,width:'90%',height:'55%'}",
                     seriesType="bars",
                     legend ="top",
                     title="Total number of check-in by days of the week ",
                     height = 700,
                     width = 1180,
                     hAxis= "{title: 'Days of the week'}",
                     vAxis= "{title: 'Total Count of Check-in '}"
                   ))

plot(gdays)



###########################################################################################

checkingtime_time = checkingTime %>%
  group_by(hours) %>%
  summarise(checkingTotal = sum(check_in_number)) 

stringToTime = function(s){
  as.integer(gsub(pattern = ':', replacement = '', x = substr(s, 1, 2)))
}

checkingtime_time['hours_int'] = sapply(checkingtime_time$hours, stringToTime)
checkingtime_time = arrange(checkingtime_time, hours_int)

gtime =gvisColumnChart(data = checkingtime_time,
                xvar = "hours",
                yvar = "checkingTotal",
                options=list(
                  chartArea=  "{left:110,top:50,width:'90%',height:'55%'}",
                  seriesType="bars",
                  legend ="top",
                  title="Total number of check-in by time of the day ",
                  height = 700,
                  width = 1180,
                  hAxis= "{title: 'Hours'}",
                  vAxis= "{title: 'Total Count of Check-in '}"
                ))

plot(gtime)


##########################################################################################33


checkingtime_dayhours = checkingTime %>%
                arrange(desc(check_in_number)) %>%
                top_n(15)
checkingtime_dayhours['dayHour'] = paste(checkingtime_dayhours$days, checkingtime_dayhours$hours)

checkingtime_dayhours %>% 
  ggvis(~dayHour, ~check_in_number) %>%
  layer_bars(width = 0.5) 

checkingtime_dayhours$dayHour <- factor(checkingtime_dayhours$dayHour, 
                                        levels=unique(checkingtime_dayhours$dayHour))

checkingtime_dayhours %>% 
  ggvis(~dayHour, ~check_in_number, fill=~days) %>%
  layer_bars(width = 0.5) 


gdaytime =gvisColumnChart(data = checkingtime_dayhours,
                       xvar = "dayHour",
                       yvar = "check_in_number",
                       options=list(
                         chartArea=  "{left:110,top:50,width:'90%',height:'55%'}",
                         seriesType="bars",
                         legend ="top",
                         title="Total number of check-in by day and time of the week ",
                         bar="{groupWidth:'40%'}",
                         colors="['#FFA500' ,'red', 'blue']",
                         height = 700,
                         width = 1180,
                         hAxes="[{title:'DayHour',
                                      textPosition: 'out'}]",
                         
                         vAxis= "{title: 'Total Count of Check-in '}"
                       ))

plot(gdaytime)



?arrange


########################################################################################333



tabItem(tabName = "LDASelect",
        div(class = "outer",visOutput('myChartLDASelect'),height ="500",width = "100%"),
        
        absolutePanel(
          id = "hover_box",
          class = "panel panel-default",
          fixed = FALSE,
          draggable = TRUE,
          top = 20,
          left = "auto",
          right = 20,
          bottom = "auto",
          width = 300,
          height = "auto",
          
          selectInput(inputId ="LDAid", label ="Input Test",
                      choices = c("American" = "json_American_Negative",
                                  "Chinese" = "json_Asian_Fusion_Negative",
                                  "Mexican" = "json"), 
                      selected = "json_American_Negative",selectize=FALSE)
          
          
        )), 

##########################################################################333

menuItem("LDAInputs", icon = icon("bar-chart-o"), tabName = "LDASelect")),
