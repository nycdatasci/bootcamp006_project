# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(ggmap)
library(maps)
library(mapdata)
setwd("~/Documents/NYCDSA/Project 1")

#
# 1- load Data: Make indicators readable and Country consistent with world map;
# remove "unit" column.
dataOECD = read.csv('~/Documents/NYCDSA/Project 1/BLI_10072016_Clean.csv',sep = ";") 
dataOECD = tbl_df(dataOECD)%>% select(.,-4) %>%
  mutate(.,Indicator = make.names(Indicator), 
         Country = ifelse(Country=='United States','USA',
                   ifelse(Country=='United Kingdom', 'UK',paste(Country))))

# 2- Data are complete for Inequality=Total, Men and Women. They are not 
# complete for High and Low. We split data and pivot the table for plotting 
# purposes.
dataOECD_TMF = dataOECD  %>%  
  filter(., Inequality %in% c("Total", "Men", "Women"))

dataOECD_THL = dataOECD  %>%  
  filter(., Inequality %in% c("Total", "High", "Low"))

#
# 3- Normalization: determine which indicators enhance well-being and normalize
# accordingly; 

is.positiveIndicator = function(x){ 
  impact = data.frame(Indicator = unique(dataOECD$Indicator),
                      Value = c(F,F,T,T,T,T,F,T,T,T,T,T,F,T,T,T,T,T,F,F,T,F,T,T)) %>%
    spread(.,Indicator,Value)
  return(impact[,x])
}

dataOECD_normalize_TMF = dataOECD_TMF %>% group_by(.,Indicator)%>%
  mutate(.,Value = ifelse(is.positiveIndicator(Indicator),
                          100*(Value - min(Value))/(max(Value) - min(Value)),
                          100*(1-(Value - min(Value))/(max(Value) - min(Value)))
  ))

dataOECD_normalize_THL = dataOECD_THL %>% group_by(.,Indicator)%>%
  mutate(.,Value = ifelse(is.positiveIndicator(Indicator),
                          100*(Value - min(Value))/(max(Value) - min(Value)),
                          100*(1-(Value - min(Value))/(max(Value) - min(Value)))
  ))

# 4- Aggregate data: transmute the new data frame, rename Country in region 
# (region = Country).
dataOECD_aggregate_TMF = dataOECD_normalize_TMF  %>%  
  spread(.,Indicator,Value) %>%
  transmute(., region = Country, Inequality=Inequality,
            Housing = (Rooms.per.person + Housing.expenditure + Dwellings.without.basic.facilities)/3,
            Wealth = (Household.net.adjusted.disposable.income + Household.net.financial.wealth)/2,
            Jobs = (Labour.market.insecurity + Personal.earnings + Long.term.unemployment.rate+Employment.rate)/4,
            Education = (Years.in.education + Student.skills + Educational.attainment)/3,
            Environment = (Water.quality + Air.pollution)/2,
            Engagement = (Stakeholder.engagement.for.developing.regulations + Voter.turnout)/2,
            Health = (Self.reported.health + Life.expectancy)/2,
            Safety = (Homicide.rate + Feeling.safe.walking.alone.at.night)/2,
            Work.life.balance = (Time.devoted.to.leisure.and.personal.care + Employees.working.very.long.hours)/2,
            Community = Quality.of.support.network,
            Life.satisfaction = Life.satisfaction
  ) 

dataOECD_aggregate_THL = dataOECD_normalize_THL  %>%  
  spread(.,Indicator,Value) %>%
  transmute(., region = Country, Inequality=Inequality,
            Housing = (Rooms.per.person + Housing.expenditure + Dwellings.without.basic.facilities)/3,
            Wealth = (Household.net.adjusted.disposable.income + Household.net.financial.wealth)/2,
            Jobs = (Labour.market.insecurity + Personal.earnings + Long.term.unemployment.rate+Employment.rate)/4,
            Education = (Years.in.education + Student.skills + Educational.attainment)/3,
            Environment = (Water.quality + Air.pollution)/2,
            Engagement = (Stakeholder.engagement.for.developing.regulations + Voter.turnout)/2,
            Health = (Self.reported.health + Life.expectancy)/2,
            Safety = (Homicide.rate + Feeling.safe.walking.alone.at.night)/2,
            Work.life.balance = (Time.devoted.to.leisure.and.personal.care + Employees.working.very.long.hours)/2,
            Community = Quality.of.support.network,
            Life.satisfaction = Life.satisfaction
  ) 
#
# 5-Prepare world map, 
# 
map.world = map_data(map="world")
base = ggplot(data = map.world, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = NA, color = "gray10")+
  coord_fixed(xlim = c(-180, 180.0),  ylim = c(-55, 90), ratio = 1.3) + 
  xlab("Longitude") + ylab("Latitude")

zoom = coord_cartesian(xlim = c(0,100))
#
# 6-Join the World map with aggregated indicators
#
map_ind_join = inner_join(map.world, 
                          filter(dataOECD_aggregate_TMF,Inequality == 'Total'), 
                          by = "region")
map.it=function(AgInd){
  base + geom_polygon(data = map_ind_join, aes_string(fill = AgInd, alpha=.5),
                      , show.legend = FALSE) +
    ggtitle(paste0(AgInd, ' ranking over OECD countries')) + 
    scale_fill_gradientn(name = "Legend", colours = rev(rainbow(3)),
                         breaks = c(0, 20, 40, 60, 80, 100)) +
    theme(legend.position = "bottom")
}
#
# 7- Looking at the direct indicators: density per indicator, to verify bias
# we want Life.satisfaction, Self.reported.health,Quality.of.support.network,
# Feeling.safe.walking.alone.at.night
#
plot.dense=function(){
  directIndicators = c('Life.satisfaction', 'Self.reported.health',
                       'Quality.of.support.network', 
                       'Feeling.safe.walking.alone.at.night')
  dataOECD_normalize_TMF %>% filter(.,Indicator %in% directIndicators) %>%
    ggplot(data = . ,aes(x = Value)) + xlab('Ranking') +
    zoom +
    geom_density(aes(color = Indicator))
}
# 
# 8- Map indicator analysis
# 
plot.aggregated = function(data,Ind1,Ind2,Ind3 = NA,filt= NA){
  if(!is.na(filt[1])){data = filter(data,Inequality %in% filt)}
  
  g = ggplot(data = data ,aes_string(x = Ind1 , y = Ind2))
  
  if (is.na(Ind3)) {g + geom_point() + zoom }
  else{g + geom_point(aes_string(color=Ind3))+ zoom + 
      theme(legend.position = "bottom")}  
}

plot.data = function(data,Ind1,Ind2,Ind3 = NA,filt= NA){
  data_tmp = data %>% spread(.,Indicator,Value)
  
  if(!is.na(filt[1])){data_tmp = filter(data_tmp,Inequality %in% filt)}
  
  g = ggplot(data = data_tmp ,aes_string(x = Ind1 , y = Ind2))
  
  if (is.na(Ind3)) {g + geom_point()}
  else{g + geom_point(aes_string(color=Ind3)) + 
      theme(legend.position = "bottom")}  
}

box.data = function(data,Ind1,Ind2=NA,filt= NA){
  data = filter(data,Indicator %in% Ind1)
  
  if(!is.na(filt[1])){data = filter(data,Inequality %in% filt)}
  
  g = ggplot(data = data ,aes(x = Indicator,y=Value)) + geom_boxplot()
  
  if (is.na(Ind2[1])) {g + geom_boxplot()}
  else{g + geom_boxplot(aes_string(color=Ind2)) +
      theme(legend.position = "bottom")}
}

map.all=function(){
  map_ind_join %>%
    gather(.,key = Indicator, value = Value,8:18) %>% group_by(.,Indicator)%>%
    base + geom_polygon(. , aes(fill = Value, alpha=.5),
                        , show.legend = FALSE) + facet_wrap( ~ Indicator) +
    scale_fill_gradientn(name = "Legend", colours = rev(rainbow(3)),
                         breaks = c(0, 20, 40, 60, 80, 100)) +
    theme(legend.position = "bottom")
}
#
# Correlation for aggregated data
#
corr_aggregate = function(){
dataOECD_aggregate_TMF %>% filter(.,Inequality == 'Total') %>% 
  select(.,-1,-2) %>% cor(.) %>%
   corrplot(corr = ., order = "hclust",method = "circle")
}

#
# Correlation for non aggregated data
# 
corr_NonAggregate_all = function(){
  dataOECD_TMF %>% filter(.,Inequality == 'Total') %>%
  spread(.,Indicator,Value) %>%
  select(.,-1,-2) %>% 
  cor(.) %>%
  corrplot(corr = ., order = "hclust",method = "circle",
         type = 'lower', tl.cex = .5)
}

corr_NonAggregate = function(fill){
  dataOECD_TMF %>% filter(.,Inequality == 'Total') %>%
    spread(.,Indicator,Value) %>%
    select(.,-1,-2) %>%
    select(.,one_of(fill)) %>%
    cor(.) %>%
    corrplot(corr = ., order = "hclust",method = "circle",
             type = 'lower', tl.cex = .7)
}