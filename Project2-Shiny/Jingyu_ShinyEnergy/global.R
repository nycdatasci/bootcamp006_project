library(plyr)
library(dplyr)
library(googleVis)
library(ggplot2)
library(reshape2)

energy_line = read.csv("general_energy_10years.csv",stringsAsFactors = FALSE) %>%
              select(.,-1)
###################################################
##for annual growth ratio
energy_line_1 = energy_line[1:(nrow(energy_line)-1),]
energy_line_2 = energy_line[2:nrow(energy_line),]
energy_vary_ratio_temp = (energy_line_2 -energy_line_1) /energy_line_1 
energy_vary_ratio = cbind(energy_line_2$Period,energy_vary_ratio_temp[,-energy_vary_ratio_temp$Period])

choice = colnames(energy_vary_ratio[,2:ncol(energy_vary_ratio)])
##############################################
# for state map with general source
energy_state = read.csv("energy_state_general.csv",stringsAsFactors = FALSE) %>%
  select(.,-1)

#############################################
#for renewable source 10 years

re_energy_line = read.csv("re_energy_10years.csv",stringsAsFactors = FALSE) %>%
  select(.,-1) %>%
  rename(.,c(Solar.Photovoltaic = "Solar Photovoltaic",Solar.Thermal = "Solar Thermal")) 

choice1 = colnames(re_energy_line[,2:ncol(re_energy_line)])

##############################################
re_energy_line_1 = re_energy_line[1:(nrow(re_energy_line)-1),]
re_energy_line_2 = re_energy_line[2:nrow(re_energy_line),]
re_energy_vary_ratio_temp = (re_energy_line_2 -re_energy_line_1) /re_energy_line_1 
re_energy_vary_ratio = cbind(re_energy_line_2$Period,re_energy_vary_ratio_temp[,-re_energy_vary_ratio_temp$Period])

################################################

re_energy_state = read.csv("re_energy_state.csv",stringsAsFactors = FALSE) %>%
  select(.,-1)%>%
  rename(.,c(Solar.Photovoltaic = "Solar Photovoltaic",Solar.Thermal = "Solar Thermal")) 
###################################################
detach("package:plyr", unload=TRUE)
# the data for planned electric generating units in 20 years
unit_add <-  read.csv("planned_unit_additions.csv",stringsAsFactors = FALSE)
countES_add <-  unit_add %>% group_by(., Energy.Source.Code) %>% summarise(., countES= n()) %>%
  filter(., Energy.Source.Code %in% c("NUC ","BIT ") | countES > 20 )

name <- data.frame(Source= c("Coal","Nuclear","Natural Gas","Petroleum", "Hydroelectric","Wind","Solar","Biomass"),
                   Energy.Source.Code = c("BIT ","NUC ","NG ","DFO ","WAT ","WND ", "SUN ","LFG "),
                   Class = c("NonRenewable","NonRenewable","NonRenewable","NonRenewable","Renewable","Renewable","Renewable","Renewable")) 

name$Source <- factor(name$Source, 
                      levels = c("Coal","Nuclear","Natural Gas","Petroleum", "Hydroelectric","Wind","Solar","Biomass"))

unit_add1 <- left_join(name, countES_add, by = "Energy.Source.Code")
unit_add1$property = "Addition"

#######################################################
#retirement
unit_retire <-  read.csv("planned_unit_retirement.csv",stringsAsFactors = FALSE)
countES_retire <-  unit_retire %>% group_by(., Energy.Source.Code) %>% summarise(., countES= n()) %>%
  filter(., Energy.Source.Code %in% name$Energy.Source.Code)

unit_retire1 <- left_join(name, countES_retire, by = "Energy.Source.Code")
unit_retire1$countES = -unit_retire1$countES
unit_retire1$property = "Retirement"

library(plyr)
unit_net <-  rbind(unit_add1,unit_retire1) %>%
  group_by(.,Source)
unit_net$Class<-  with(unit_net, interaction(property,Class)) 

unit_net2 <- unit_net %>% select(., c(Source,property,countES)) %>% dcast(., Source~property)



