
library(ggmap)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(reshape2) 
#library(gridExtra)
#library(Rmisc)
library(plotrix)

setwd("C:/Users/Samriddhi/Desktop/")
states = map_data("state")
MS_66_14 = read.csv("R_Folder/Practice/GunViolenceProject/MSA/Gundeaths_copy.csv")


#-----------------------------------------------------------------------------------------------
          #data Preparation

#mass shooting_state
MS_66_14_State = MS_66_14 %>% 
                group_by(State) %>%  
                summarise( Total_Incidents = n() ,
                           Deaths = sum(Number.of.Victim.Fatalities),
                           Injuries = sum(Number.of.Victims.Injured)
                           ) %>% 
                arrange(State)
MS_66_14_State$State = tolower(MS_66_14_State$State) 


#merging in map
map_MS_66_14_state = merge(states, MS_66_14_State, 
                           by.x='region',by.y='State', all=T) # merging with spatial DB 
map_MS_66_14_state= map_MS_66_14_state[order(map_MS_66_14_state$order),]

#------------------------------------------------------------------------------------------------
# mapping lat long of mass shooting
ggplot() + geom_polygon(data = map_MS_66_14_state, aes(x = long, y = lat, group=group),
                        fill="grey40",  colour="grey90", size = 0.1)  + 
          geom_point(mapping = aes(x = Longitude, y = Latitude ), data = MS_66_14, 
                     na.rm = T, show.legend = T,  inherit.aes = TRUE, colour = "red", 
                     size = 1) + coord_fixed(1.3)

# Mass Shooting Incident by state since 1966
ggplot(data = map_MS_66_14_state, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = cut_number(as.numeric(Total_Incidents),5))) +
#  geom_point(mapping = aes(x = Longitude, y = Latitude ), data = MS_66_14, 
                  #   colour = "red", size = 1) +
  geom_path(colour = 'gray') +
  scale_fill_brewer('Incidents', palette  = 'YlGnBu') +
  ggtitle("Mass Shooting Incidents in US (1966 - 2016)")+
  xlab("") +
  ylab("") 

# Mass Shooting Deaths by state since 1966
ggplot(data = map_MS_66_14_state, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = cut_number(as.numeric(Deaths),5))) +
  geom_path(colour = 'gray') +
  scale_fill_brewer('Deaths', palette  = 'YlGnBu') +
  ggtitle("Mass Shooting Deaths in US (1966 - 2016)")+
  xlab("") +
  ylab("")


# Mass Shooting Injuries by state since 1966
ggplot(data = map_MS_66_14_state, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = cut_number(as.numeric(Injuries),5))) +
  geom_path(colour = 'gray') +
  scale_fill_brewer('Injuries', palette  = 'YlGnBu') +
  ggtitle("Mass Shooting Injuries in US (1966 - 2016)")+
  xlab("") +
  ylab("")


#------------------------------------------------------------------------------------------------  

                                #Gun type used, incident Deaths Injuries
colnames(MS_66_14)
MS_66_14$TypeofGunGeneral = tolower(MS_66_14$TypeofGunGeneral) 

# group by gun type
MS_66_14_GunType = MS_66_14 %>% 
  rename(TypeOfGun = TypeofGunGeneral ) %>%
  group_by(TypeOfGun) %>%  
  summarise( Incidents = n() ,
             Deaths = sum(Number.of.Victim.Fatalities),
             Injuries = sum(Number.of.Victims.Injured)
  )
MS_66_14_GunType
colnames(MS_66_14_GunType)

melt_MS_66_14_GunType = melt(MS_66_14_GunType, id.vars = 'TypeOfGun', 
                        measure.vars = c("Incidents", "Deaths", "Injuries"),
                        variable.name = "statistics") %>%
                        arrange(TypeOfGun)

g_GunType = ggplot(data = melt_MS_66_14_GunType, aes(x = TypeOfGun,
                                                     y = value,
                                                     fill=statistics)  ) + 
  geom_bar(stat = "identity", position='dodge')  +
  geom_text(aes(label = value), stat = "identity",
            position = position_dodge(width=1),vjust=-0.20, size = 4) 
# theme(axis.text.x = element_text(angle=90))
#?geom_text
g_GunType + coord_flip() +
  ggtitle("Gun Type Statistics")+
  xlab("Type of Gun") +
  ylab("Frequency")+
  theme(legend.background = element_rect(), legend.position=c(0.8,0.8),
        legend.justification=c(0.6,0.4), 
        legend.title = element_text(colour="blue", size=12, face="bold"))

                #Gun type used, incident Deaths Injuries

#----------------------------------------------------------------------------------------------

# Gun incident Deaths Injuries  by Sex
colnames(MS_66_14)


# group by sex
MS_66_14_Sex = MS_66_14 %>% 
  group_by(ShooterSex) %>%  
  summarise( Incidents = n() ,
             Deaths = sum(Number.of.Victim.Fatalities),
             Injuries = sum(Number.of.Victims.Injured)
  )
MS_66_14_Sex

#melting
melt_MS_66_14_sex = melt(MS_66_14_Sex, id.vars = 'ShooterSex', 
                             measure.vars = c("Incidents", "Deaths", "Injuries"),
                             variable.name = "statistics") 
                            

g_ShooterSex = ggplot(data = melt_MS_66_14_sex, aes(x = ShooterSex,
                                                     y = value,  fill=statistics)  ) + 
               geom_bar(stat = "identity", position='dodge') +
              geom_text(aes(label = value), stat = "identity",
                  position = position_dodge(width = 1), size = 4) 
# theme(axis.text.x = element_text(angle=90))
g_ShooterSex + coord_flip() +
  ggtitle("Sex of the Shooter")+
  xlab("Sex") +
  ylab("Frequency")+
  theme(legend.background = element_rect(), legend.position=c(0.8,0.8),
        legend.justification=c(0.6,0.4), 
        legend.title = element_text(colour="blue", size=12, face="bold"))

# incident Deaths Injuries by SEX

#----------------------------------------------------------------------------------------------



# Gun incident Deaths Injuries  by race
colnames(MS_66_14)


# group by Race
MS_66_14_Race = MS_66_14 %>% 
  group_by(ShooterRace) %>%  
  summarise( Incidents = n() ,
             Deaths = sum(Number.of.Victim.Fatalities),
             Injuries = sum(Number.of.Victims.Injured)
  )
MS_66_14_Race

#melting
melt_MS_66_14_Race = melt(MS_66_14_Race, id.vars = 'ShooterRace', 
                         measure.vars = c("Incidents", "Deaths", "Injuries"),
                         variable.name = "statistics") 


g_ShooterRace = ggplot(data = melt_MS_66_14_Race, aes(x = ShooterRace,
                                                    y = value,  fill=statistics)  ) + 
  geom_bar(stat = "identity", position='dodge')  +
  geom_text(aes(label = value), stat = "identity",
            position = position_dodge(width = 1), size = 4) 
# theme(axis.text.x = element_text(angle=90))
g_ShooterRace + coord_flip() +
  ggtitle("Race of the Shooter")+
  xlab("Race") +
  ylab("Frequency")+
  theme(legend.background = element_rect(), legend.position=c(0.8,0.1),
        legend.justification=c(0.6,0.4), 
        legend.title = element_text(colour="blue", size=12, face="bold"))

# incident Deaths Injuries by Race


#----------------------------------------------------------------------------------------------



# Gun incident Deaths Injuries  by Place Type
colnames(MS_66_14)
MS_66_14$Place.Type = tolower(MS_66_14$Place.Type) 


# group by Place Type
MS_66_14_PlaceType = MS_66_14 %>% 
  group_by(Place.Type) %>%  
  summarise( Incidents = n() ,
             Deaths = sum(Number.of.Victim.Fatalities),
             Injuries = sum(Number.of.Victims.Injured)
  ) %>%
  arrange(desc(Incidents)) %>%
  head(10)

MS_66_14_PlaceType

#melting
melt_MS_66_14_PlaceType = melt(MS_66_14_PlaceType, id.vars = 'Place.Type', 
                          measure.vars = c("Incidents", "Deaths", "Injuries"),
                          variable.name = "statistics") 


g_PlaceType = ggplot(data = melt_MS_66_14_PlaceType, aes(x = Place.Type,
                                                      y = value,  fill=statistics)  ) + 
  geom_bar(stat = "identity", position='dodge') +
  geom_text(aes(label = value), stat = "identity",
            position = position_dodge(width = 1), size = 4) 
# theme(axis.text.x = element_text(angle=90))
g_PlaceType + coord_flip() +
  ggtitle("Mass Shooting by Type of Place")+
  xlab("Type of Place") +
  ylab("Frequency")+
  theme(legend.background = element_rect(), legend.position=c(0.8,0.1),
        legend.justification=c(0.6,0.4), 
        legend.title = element_text(colour="blue", size=12, face="bold"))

# incident Deaths Injuries by Place Type

#----------------------------------------------------------------------------------------------



# Gun incident Deaths Injuries  by Motive
colnames(MS_66_14)
MS_66_14$Place.Type = tolower(MS_66_14$Place.Type) 


# group by Motive
MS_66_14_Motive = MS_66_14 %>% 
  group_by(PossibleMotiveGeneral) %>%  
  summarise( Incidents = n() ,
             Deaths = sum(Number.of.Victim.Fatalities),
             Injuries = sum(Number.of.Victims.Injured)
  ) %>%
  arrange(desc(Incidents))%>%
  head(10)

MS_66_14_Motive

#melting
melt_MS_66_14_Motive = melt(MS_66_14_Motive, id.vars = 'PossibleMotiveGeneral', 
                               measure.vars = c("Incidents", "Deaths", "Injuries"),
                               variable.name = "statistics") 


g_Motive = ggplot(data = melt_MS_66_14_Motive, aes(x = PossibleMotiveGeneral,
                                                         y = value,  fill=statistics)  ) + 
  geom_bar(stat = "identity", position='dodge') +
  geom_text(aes(label = value), stat = "identity",
            position = position_dodge(width = 1), size = 4) 
# theme(axis.text.x = element_text(angle=90))
g_Motive + coord_flip() +
  ggtitle("Mass Shooting by Motive")+
  xlab("Motive") +
  ylab("Frequency")+
  theme(legend.background = element_rect(), legend.position=c(0.8,0.1),
        legend.justification=c(0.6,0.4), 
        legend.title = element_text(colour="blue", size=12, face="bold"))

# incident Deaths Injuries by Motive

#--------------------------------------------------------------------------------------------

# incident Deaths Injuries by Age Range

# the data is not correct for age

class(MS_66_14$ShooterAge)
MS_66_14$ShooterAge = as.numeric(MS_66_14$ShooterAge ) # converting from factor to numeric

MSS_66_14_Age =MS_66_14 %>%
              mutate(AgeRange = ifelse(ShooterAge <= 10, "0-10",
                                ifelse(ShooterAge > 10 & ShooterAge <= 20, "10-20",
                                ifelse(ShooterAge > 20 & ShooterAge <= 30, "20-30",
                                ifelse(ShooterAge > 30 & ShooterAge <= 40, "30-40",
                                ifelse(ShooterAge > 40 & ShooterAge <= 50, "40-50",
                                ifelse(ShooterAge > 50 & ShooterAge <= 60, "50-60",
                                ifelse(ShooterAge > 60, "Greater than 60",
                                               "Unknown" ))))))))


# group by Motive
colnames(MSS_66_14_Age)
MSS_66_14_Age_groupby = MSS_66_14_Age %>% 
  group_by(AgeRange) %>%  
  summarise( Incidents = n() 
  )


ggplot(data = MSS_66_14_Age, aes(x = AgeRange)) +
  geom_bar(aes(fill = ShooterSex) )

#---------------------------------------------------------------------------------------------
