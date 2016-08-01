# FILE & LIBRARY READ_INS
library(leaflet)

data = read.csv("./data/Dat.Reduced.csv", header = TRUE, stringsAsFactors = FALSE)
data = data[ , -which(names(data) %in% c("nkill"))]

### Creating Marker variable for Global Map circle colors
# data = data %>% mutate(Marker = ifelse(success == 1 & INT.LOG==1, "Red",
#                            ifelse(success == 1 & (INT.IDEO == 1 | INT.MISC == 1), "Pink",
#                                   ifelse(success == 0, "Green", "Blue"))))
# data$Marker = as.factor(data$Marker)
# pal <- colorFactor(c("blue", "red", "green", "purple"), domain = c("blue", "red", "green", "pink"))


### Melt IDs for stacking the data
melt.ids =  c("eventid", "iyear", "imonth", "iday", "country", "country_txt", "region", 
              "region_txt", "provstate", "city", "latitude", "longitude", "summary")

## CREATING VARIABLES FOR OVERALL DISTRIBUTION ON INTRO TAB
Annual.Change = data %>% group_by(iyear)  %>% summarise(occurences = n()) %>% arrange(iyear)
Annual.Change = Annual.Change %>% mutate(change = ((occurences-lag(occurences, n = 1L))/lag(occurences, n = 1L)*100)) 
annual.change.labels = Annual.Change %>% filter(iyear == 1998 | iyear == 2008 | iyear == 2012)


#CREATING FILTER VALUES AND SELECTION OPTIONS
year.set = unique(data$iyear)
country.set = sort(unique(data$country_txt))
region.set = unique(data$region_txt)

International = c("INT.IDEO", "INT.LOG", "INT.MISC")
names(International) = c("Ideologically International (Perpetrator Nationality Vs. Victim Nationality)", "Logistically Internationally (Perpetrator Nationality Vs. Location)", "Other International (Location Vs. Victim Nationality)")

TargetChoices = c("tgt.abortion", "tgt.air", "tgt.business", "tgt.citizens", "tgt.educational", "tgt.foodwater", 
                  "tgt.govt.diplomatic", "tgt.govt.general", "tgt.maritime", "tgt.media", "tgt.military", "tgt.ngo", 
                   "tgt.police", "tgt.religious", "tgt.tourists", "tgt.transportation", "tgt.utility")

names(TargetChoices) = c("Abortion Facilities", "Air", "Business", "Citizens", "Educational", "Foodwater", 
                         "Govt: Diplomatic", "Govt: General", "Maritime", "Media", "Military",  "NGO",
                         "Police", "Religious", "Tourists", "Transportation", "Utility")

WeaponFilters = c("wp.biological", "wp.chemical", "wp.explosives",  "wp.fakeweapons", "wp.firearms", "wp.incendiary", 
                  "wp.melee", "wp.sabotage.equipment", "wp.radiological", "wp.vehicle")

names(WeaponFilters) = c("Biological", "Chemical", "Explosives", "Fake Weapons", "Firearms", "Incendiary", 
                         "Melee", "Sabotage Equipment", "Radiological", "Vehicle")

AttackTypeFilter = c("at.armedassault", "at.assassination", "at.BombingExplosion", "at.facility", "at.hijacking",  
                     "at.hostagekidnap", "at.hostagetaking","at.unarmedassault")

names(AttackTypeFilter) = c("Armed Assault", "Assassination", "Bombing", "Facility", "Hijacking",  
                            "Hostage Kidnap", "Hostage Taking","Unarmed Assault")

radioChoices =  c("INT.IDEO", "INT.LOG", "INT.MISC", "tgt.abortion", "tgt.air", "tgt.business", "tgt.citizens", "tgt.educational", 
                  "tgt.foodwater", "tgt.govt.diplomatic", "tgt.govt.general", "tgt.maritime", "tgt.media", "tgt.military", "tgt.ngo", 
                  "tgt.police", "tgt.religious", "tgt.tourists", "tgt.transportation", "tgt.utility", "wp.biological", "wp.chemical", 
                  "wp.explosives",  "wp.fakeweapons", "wp.firearms", "wp.incendiary", "wp.melee", "wp.sabotage.equipment", 
                  "wp.radiological", "wp.vehicle", "at.armedassault", "at.assassination", "at.BombingExplosion", "at.facility", 
                  "at.hijacking", "at.hostagekidnap", "at.hostagetaking","at.unarmedassault")

names(radioChoices) = c("Ideologically International", "Logistically Internationally", "Other International", "Target: Abortion Facilities",
                        "Target: Air", "Target: Business", "Target: Citizens", "Target: Educational", "Target: Foodwater", 
                        "Target: Govt: Diplomatic", "Target: Govt: General", "Target: Maritime", "Target: Media", "Target: Military",  
                        "Target: NGO", "Target: Police", "Target: Religious", "Target: Tourists", "Target: Transportation", 
                        "Target: Utility", "Weapon: Biological", "Weapon: Chemical", "Weapon: Explosives", "Weapon: Fake Weapons", 
                        "Weapon: Firearms", "Weapon: Incendiary", "Weapon: Melee", "Weapon: Sabotage Equipment", "Weapon: Radiological", 
                        "Weapon: Vehicle", "Type of Attack: Armed Assault", "Type of Attack: Assassination", "Type of Attack: Bombing", 
                        "Type of Attack: Facility", "Type of Attack: Hijacking",  "Type of Attack: Hostage Kidnap", 
                        "Type of Attack: Hostage Taking","Type of Attack: Unarmed Assault")
