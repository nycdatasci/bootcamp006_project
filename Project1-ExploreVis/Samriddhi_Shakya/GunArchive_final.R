

library(ggmap)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(reshape2) 
library(gridExtra)
#library(Rmisc)
library(plotrix)
library(grid)


setwd("C:/Users/Samriddhi/Desktop/")

#----------------------------------------------------------------------------------------------

# Data Preparation
# converting address into Lat / Long

                      # year 2014

MS2014= read.csv("R_Folder/Practice/GunViolenceProject/GunViolenceArchive/MassShooting2014.csv",stringsAsFactors = FALSE)

MS2014$FullAddress = paste (MS2014$Address,MS2014$City.Or.County, sep = ",")
MS2014 = MS2014[MS2014$State != "Alaska", ]
MS2014 = MS2014[MS2014$State != "Hawaii", ]

for (i in 1:nrow(MS2014)) {
  latlon = geocode(MS2014[i,8])
  MS2014$lon[i] = as.numeric(latlon[1])
  MS2014$lat[i] = as.numeric(latlon[2])
} # The following is a simple for loop that runs over each state and returns lat/lon coordinates:

MS2014 = MS2014[MS2014$lon < 0 ]
# MS2014 = MS2014[!(is.na(MS2014$lon)),]
# which(is.na(MS2014$lon))

write.csv(MS2014, file = "C:/Users/Samriddhi/Desktop/R_Folder/Practice/GunViolenceProject/GunViolenceArchive/Exportedcsv/MS2014.csv")

                                    # year 2014
          #++++++++++++++++++++++++++++++++++++++++++++++++++++++===

                                    # year 2015

MS2015= read.csv("R_Folder/Practice/GunViolenceProject/GunViolenceArchive/MassShooting2015.csv",stringsAsFactors = FALSE)
MS2015$FullAddress = paste (MS2015$Address,MS2015$City.Or.County, sep = ",")
MS2015 = MS2015[MS2015$State != "Alaska", ]
MS2015 = MS2015[MS2015$State != "Hawaii", ]

for (i in 1:nrow(MS2015)) {
  latlon = geocode(MS2015[i,8])
  MS2015$lon[i] = as.numeric(latlon[1])
  MS2015$lat[i] = as.numeric(latlon[2])
} # The following is a simple for loop that runs over each state and returns lat/lon coordinates:

MS2015 = MS2015[MS2015$lon < 0,  ]
# MS2015 = MS2015[!(is.na(MS2015$lon)),]
# which(is.na(MS2015$lon))

write.csv(MS2015, file = "C:/Users/Samriddhi/Desktop/R_Folder/Practice/GunViolenceProject/GunViolenceArchive/Exportedcsv/MS2015.csv")

                                        # year 2015

      #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                                        # year 2016

MS2016= read.csv("R_Folder/Practice/GunViolenceProject/GunViolenceArchive/MassShooting2016.csv",stringsAsFactors = FALSE)
MS2016$FullAddress = paste (MS2016$Address,MS2016$City.Or.County, sep = ",")
MS2016 = MS2016[MS2016$State != "Alaska", ]
MS2016 = MS2016[MS2016$State != "Hawaii", ]

for (i in 1:nrow(MS2016)) {
  latlon = geocode(MS2016[i,8])
  MS2016$lon[i] = as.numeric(latlon[1])
  MS2016$lat[i] = as.numeric(latlon[2])
} # The following is a simple for loop that runs over each state and returns lat/lon coordinates:


#MS2016 = MS2016[!(is.na(MS2016$lon)),]
#which(is.na(MS2015$lon))

write.csv(MS2016, file = "C:/Users/Samriddhi/Desktop/R_Folder/Practice/GunViolenceProject/GunViolenceArchive/Exportedcsv/MS2016.csv")

                                          # year 2016
          
                  # Data Preparation
#-----------------------------------------------------------------------------------------------

# Reading data from new csv file created 

MS_2016 = read.csv("C:/Users/Samriddhi/Desktop/R_Folder/Practice/GunViolenceProject/GunViolenceArchive/Exportedcsv/MS2016.csv",stringsAsFactors = FALSE)

MS_2015 = read.csv("C:/Users/Samriddhi/Desktop/R_Folder/Practice/GunViolenceProject/GunViolenceArchive/Exportedcsv/MS2015.csv",stringsAsFactors = FALSE)

MS_2014 = read.csv("C:/Users/Samriddhi/Desktop/R_Folder/Practice/GunViolenceProject/GunViolenceArchive/Exportedcsv/MS2014.csv",stringsAsFactors = FALSE)


#---------------------------------------------------------------------------------------------

#Plotting the coordinates in map


states = map_data("state")

#2014
g_MS2014 = ggplot() + 
            geom_polygon(data = states, aes(x = long, y = lat, group=group),
                         fill="grey60",  colour="grey90", size = 0.1) +
            geom_point(aes(x=lon, y=lat), data=MS_2014, col="red", alpha=1,size =2 )  +
            ggtitle("Mass Shooting in US (2014)")+
            xlab("") +
            ylab("")
g_MS2014

#2015
g_MS2015 =ggplot() + 
          geom_polygon(data = states, aes(x = long, y = lat, group=group),
                       fill="grey60",  colour="grey90", size = 0.1) +
          geom_point(aes(x=lon, y=lat), data=MS_2015, col="red", alpha=1,size =2 ) +
          ggtitle("Mass Shooting in US (2015)")+
          xlab("") +
          ylab("")
g_MS2015

#2016
g_MS2016= ggplot() + 
          geom_polygon(data = states, aes(x = long, y = lat, group=group),
                       fill="grey60",  colour="grey90", size = 0.1) +
          geom_point(aes(x=lon, y=lat), data=MS_2016, col="red", alpha=1,size =2 ) +
          ggtitle("Mass Shooting in US (2016)")+
          xlab("") +
          ylab("")
g_MS2016

#library(Rmisc)
#multiplot(g_MS2014, g_MS2015, g_MS2016,  cols=2)
#?multiplot

#----------------------------------------------------------------------------------------------
                            #HeatMap

#2014
g_HeatMap2014= ggplot(aes(x=lon,y=lat), data=MS_2014) + 
            geom_polygon(data = states, aes(x = long, y = lat, group=group),
                         fill="grey60",  colour="grey90", size = 0.1) +
            geom_point(aes(x=lon, y=lat), data=MS_2014, col="red", alpha=1,size =2 ) +
            geom_density2d(data = MS_2014, aes(x = lon, y = lat)) +
            stat_density2d( aes(fill = ..level.., alpha = ..level..),
                            size = 0.01, bins = 20, geom = 'polygon')  + 
            scale_fill_gradientn(colours=rev(brewer.pal(10,"Spectral"))) +
            scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
            ggtitle("Heat Map in US (2014)")+
            xlab("") +
            ylab("")

g_HeatMap2014

#2015
g_HeatMap2015 = ggplot(aes(x=lon,y=lat), data=MS_2015) + 
              geom_polygon(data = states, aes(x = long, y = lat, group=group),
                           fill="grey60",  colour="grey90", size = 0.1) +
              geom_point(aes(x=lon, y=lat), data=MS_2015, col="red", alpha=1,size =2 ) +
              geom_density2d(data = MS_2015, aes(x = lon, y = lat)) +
              stat_density2d( aes(fill = ..level.., alpha = ..level..),
                              size = 0.01, bins = 20, geom = 'polygon')  + 
              scale_fill_gradientn(colours=rev(brewer.pal(10,"Spectral"))) +
              scale_alpha(range = c(0.00, 0.25), guide = FALSE)  +
              ggtitle("Heat Map in US (2015)")+
              xlab("") +
              ylab("")
g_HeatMap2015

#2016

#2016
g_HeatMap2016 = ggplot(aes(x=lon,y=lat), data=MS_2016) + 
              geom_polygon(data = states, aes(x = long, y = lat, group=group),
                           fill="grey60",  colour="grey90", size = 0.1) +
              geom_point(aes(x=lon, y=lat), data=MS_2016, col="red", alpha=1,size =2 ) +
              geom_density2d(data = MS_2016, aes(x = lon, y = lat)) +
              stat_density2d( aes(fill = ..level.., alpha = ..level..), 
                              size = 0.01, bins = 20, geom = 'polygon')  + 
              scale_fill_gradientn(colours=rev(brewer.pal(10,"Spectral"))) +
              scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
              ggtitle("Heat Map in US (2016)")+
              xlab("") +
              ylab("")

g_HeatMap2016
# multiplot(g_HeatMap2014, g_HeatMap2015, g_HeatMap2016,  cols=2)

                                          #Head Map
#----------------------------------------------------------------------------------------------


                                # Mass Shooting by States


#str(MS_2014)
#2014
MS_2014_state = MS_2014 %>% 
  group_by(State) %>% 
  summarise(Total_killed =sum(Killed), Total_injured = sum(Injured)) %>%
  select(State,Total_killed_2014 = Total_killed,
         Total_injured_2014 = Total_injured)


MS_2014_state$State = tolower(MS_2014_state$State) # converting state to lower case

# Merging State map with total gundeaths in each states

map_MS_2014_State = merge(states, MS_2014_state, by.x='region',by.y='State', all=T)
map_MS_2014_State= map_MS_2014_State[order(map_MS_2014_State$order),]

ggplot(data = map_MS_2014_State, aes(x = long, y = lat, group = group)) +
          geom_polygon(aes(fill = cut_number(as.numeric(Total_killed_2014),5))) +
          geom_path(colour = 'gray') +
          scale_fill_brewer('Deaths', palette  = 'YlGnBu')  +
          ggtitle("Mass Shooting Deaths in US (2014)")+
          xlab("") +
          ylab("")



#2015
MS_2015_state = MS_2015 %>% 
  group_by(State) %>%
  summarise(Total_killed =sum(Killed), Total_injured = sum(Injured)) %>%
  select(State,Total_killed_2015 = Total_killed,
         Total_injured_2015 = Total_injured)

MS_2015_state$State = tolower(MS_2015_state$State) # converting state to lower case

# Merging State map with total gundeaths in each states
map_MS_2015_State = merge(states, MS_2015_state, by.x='region',by.y='State', all=T)
map_MS_2015_State= map_MS_2015_State[order(map_MS_2015_State$order),]

ggplot(data = map_MS_2015_State, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = cut_number(as.numeric(Total_killed_2015),5))) +
  geom_path(colour = 'gray') +
  scale_fill_brewer('Deaths', palette  = 'YlGnBu') +
  ggtitle("Mass Shooting Deaths in US (2015)")+
  xlab("") +
  ylab("")



#2016
MS_2016_state = MS_2016 %>%
                group_by(State) %>% 
                summarise(Total_killed =sum(Killed), Total_injured = sum(Injured)) %>%
                select(State, Total_killed_2016 = Total_killed, 
                       Total_injured_2016 = Total_injured)

MS_2016_state$State = tolower(MS_2016_state$State) # converting state to lower case

# Merging State map with total gundeaths in each states
map_MS_2016_State = merge(states, MS_2016_state, by.x='region',by.y='State', all=T)
map_MS_2016_State= map_MS_2016_State[order(map_MS_2016_State$order),]

ggplot(data = map_MS_2016_State, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = cut_number(as.numeric(Total_killed_2016),5))) +
  geom_path(colour = 'gray') +
  scale_fill_brewer('Deaths', palette  = 'YlGnBu') +
  ggtitle("Mass Shooting Deaths in US (2016)")+
  xlab("") +
  ylab("")


                            # Mass Shooting by States

#----------------------------------------------------------------------------------------------

                        # death and injury for all states

#2014
melt_MS_2014_state = melt(MS_2014_state, id.vars = 'State', 
                          measure.vars = c("Total_killed_2014", "Total_injured_2014")) %>%
                      arrange(State)

g2014 = ggplot(data = melt_MS_2014_state, aes(x = State, y = value, fill=variable)  ) + 
        geom_bar(stat = "identity", position='dodge') 
       # theme(axis.text.x = element_text(angle=90))
g2014 + coord_flip() +
        ggtitle("Mass Shooting Deaths and Injury in US (2014)")+
        xlab("Number of People") +
        ylab("States")+
        theme(legend.background = element_rect(), legend.position=c(0.8,0.8),
              legend.justification=c(0.6,0.4), 
              legend.title = element_text(colour="blue", size=12, face="bold"))


#2015
melt_MS_2015_state = melt(MS_2015_state, id.vars = 'State', 
                          measure.vars = c("Total_killed_2015", "Total_injured_2015")) %>%
                          arrange(State)

g2015 = ggplot(data = melt_MS_2015_state, aes(x = State, y = value, fill=variable)  ) + 
         geom_bar(stat = "identity", position='dodge') 
# theme(axis.text.x = element_text(angle=90))
g2015 + coord_flip() +
  ggtitle("Mass Shooting Deaths and Injury in US (2015)")+
  xlab("Number of People") +
  ylab("States")+
  theme(legend.background = element_rect(), legend.position=c(0.8,0.8),
        legend.justification=c(0.6,0.4), 
        legend.title = element_text(colour="blue", size=12, face="bold"))

#2016
melt_MS_2016_state = melt(MS_2016_state, id.vars = 'State', 
                          measure.vars = c("Total_killed_2016", "Total_injured_2016")) %>%
                          arrange(State)

g2016 = ggplot(data = melt_MS_2016_state, aes(x = State, y = value, fill=variable)  ) + 
        geom_bar(stat = "identity", position='dodge') 
# theme(axis.text.x = element_text(angle=90))
g2016 + coord_flip() +
  ggtitle("Mass Shooting Deaths and Injury in US (2016)")+
  xlab("Number of People") +
  ylab("States")+
  theme(legend.background = element_rect(), legend.position=c(0.8,0.8),
        legend.justification=c(0.6,0.4), 
        legend.title = element_text(colour="blue", size=12, face="bold"))

                            # death and injury for all states

#-----------------------------------------------------------------------------------------------

#pyramid

# joining 2014 & 2015
# 2 plots in 1

MS_2014_2015_state = full_join(MS_2014_state, MS_2015_state, by = "State")
MS_2014_2015_state

#Gundeaths comparison
pyramid.plot(MS_2014_2015_state$Total_killed_2014,
             MS_2014_2015_state$Total_killed_2015,
             labels=MS_2014_2015_state$State,top.labels=c("2014","States","2015"),
             main="Gundeaths", gap=6, unit="Number of deaths", lxcol = "red",
             rxcol = "pink",space = 0.3, ppmar=c(4,2,4,2), 
             laxlab=seq(from = 0, to = 50, by = 10),
             raxlab=seq(from = 0, to = 50, by = 10))

#Gun Injury comparison
pyramid.plot(MS_2014_2015_state$Total_injured_2014,
             MS_2014_2015_state$Total_injured_2015,
             labels=MS_2014_2015_state$State,top.labels=c("2014","States","2015"),
             main="Gun Injury", gap=22, unit="Number of deaths", lxcol = "red",
             rxcol = "pink",space = 0.3, ppmar=c(4,2,4,2), 
             laxlab=seq(from = 0, to = 200, by = 20),
             raxlab=seq(from = 0, to = 200, by = 20))


#----------------------------------------------------------------------------------------------





colnames(MS_2014_2015_state)

#melting 2014
dm1 =   melt(MS_2014_2015_state, id.vars = 'State', 
             measure.vars = c("Total_killed_2014", "Total_injured_2014"), 
             variable.name = "y_2014") %>% 
             arrange(State) %>% 
             mutate(Outcome = ifelse(y_2014=="Total_killed_2014",
                                           "Total_killed", "Total_injured" ))
#melting 2015
dm2 =   melt(MS_2014_2015_state, id.vars = 'State',
             measure.vars = c("Total_killed_2015", "Total_injured_2015"), 
             variable.name = "y_2015") %>%
             arrange(State) %>% 
             mutate(Outcome = ifelse(y_2015 =="Total_killed_2015",
                                           "Total_killed", "Total_injured" ))

#merging 2014 and 2015
melt_2014_2015_DI  = merge(dm1, dm2, 
                           by.x=c("State","Outcome"), 
                           by.y=c("State","Outcome")) %>%
                    select(State,Outcome, y_2014 = value.x, y_2015 = value.y ) 


g2014_2015_DI= ggplot(data = melt_2014_2015_DI, aes(x = State )) +
  geom_bar(aes(y=y_2014, fill=Outcome), stat="identity") +
  geom_bar(aes(y= -y_2015, fill=Outcome), stat="identity") +
  geom_hline(yintercept=0, colour="white", lwd=1) +  coord_flip(ylim=c(-220,220)) +
  scale_y_continuous(breaks=seq(-225,225,25) , 
                     labels = c('225','200','175','150','125','100','75','50','25','0','25','50'                      ,'75','100','125','150','175','200','225')) +
  labs(y="Numbers of victims", x="State") +
  ggtitle("2014                                                 2015")

g2014_2015_DI











