setwd('C:/Users/Charles/OneDrive/Data Visualization Homework/Exploratory Visualization')

library(dplyr)
library(ggplot2)
library(mapproj)
library(maps)

#Read data

statemapper = map_data("state")
RRGIdata = read.csv("RRGI_Quarterly_Data.csv")
statevec = c('CT', 'DE','MA', 'MD', 'ME', 'NH', 'NJ', 'NY', 'RI', 'VT')
lowerstate = c('connecticut', 'delaware', 'massachusetts', 'maryland', 'maine', 'new hampshire', 'new jersey', 'new york', 'rhode island', 'vermont')

#Create Table with CO2 Data
RRGI_table = select(RRGIdata, State, Year, Quarter, CO2..short.tons., Source.Category) %>%
  mutate(State = as.character(State),
         Source.Category = as.character(Source.Category))

RRGI_Summaries = RRGI_table #used for later dist, bar, and line graphs
#Switch that matches state with lowercase naming system, as in the map data

RRGI_table$State = sapply(RRGI_table$State, 
                             function(x) switch(match(x, statevec), 
                                                'connecticut', 'delaware', 'massachusetts', 'maryland', 'maine', 'new hampshire', 'new jersey', 'new york', 'rhode island', 'vermont')
)

COtable = lowerstate

#for loop that creates table with every quarter as a separate column

for(y in 2009:2016){
  for(q in 1:4){
    
    if (y == 2016 & q == 3){break}
    
    temptable = filter(RRGI_table, Year == y, Quarter ==q) %>%
      group_by(State, Year, Quarter) %>%
      summarise(Total.CO2 = sum(CO2..short.tons., na.rm = TRUE))
    
    #new jersey left RRGI program in 2012, so won't be included in sum
    if(nrow(temptable) == 9){
      temptable[10,1] = 'new jersey'
      temptable[10,4] = 0
    }
    
    temptable = arrange(temptable, State)
    
    names(temptable)[4] = paste(as.character(y), as.character(q), sep = 'Q')    
    COtable = cbind(COtable, temptable[4])
  }
}

#Create a percents table
percents = ceiling(COtable[-1]/max(COtable[-1]) * 100)
percents = cbind(lowerstate, percents)
shades = colorRampPalette(c("white", "red"))(100)

#crossref shades with percents in certain quarter, to be looped. merge test and state

colorcol = statemapper[5]
names(colorcol)[1] = 'lowerstate' 

#fills = cbind(lowerstate, shades[percents$'2009Q1'])
#colorcol = left_join(colorcol, fills, by ='lowerstate', copy = TRUE)
colorcol = left_join(colorcol, percents, by ='lowerstate', copy = TRUE)
#result is percentages for all states. Redo so colors are instead.
#fills = shades[colorcol$'2009Q1']

legtext <- c("14521 or less",
                 "10891 or less",
                 "7260 or less",
                 "3630 or less",
                 "0 or more")

statePlot = ggplot(data = statemapper, aes(x = long, y = lat))

percofmax.ktons.CO2 = 100 - colorcol[30]

CO2map = function(year, quarter) {
    percofmax.ktons.CO2 = 100 - colorcol[(year-2009)*4+quarter+1] # function worked an hour ago, not sure why not working now.

    statePlot + 
      geom_polygon(aes(group = group, fill = percofmax.ktons.CO2)) + 
      xlim(-85,-65) + ylim(35, 50) + 
      ggtitle(paste("Carbon Dioxide Emissions", paste(year, quarter, sep = 'Q')))
}

CO2map(2016, 1)

# + 
#  theme(legend.position = 'none')
#  guides(fill = guide_legend(title = 'ktons of CO2')) +
#  annotate('text', x = -70, y = c(50, label = legtext)

#legend("topright",
#       legend = legtext,
#       fill = shades[c(1, 25, 50, 75, 100)],
#       title = "ABCD")

#Can't get the legend to work... @___@

RRGI_Line = filter(RRGI_Summaries, State != 'NJ') %>%
  group_by(Year, Quarter) %>% 
  summarise(CO2.tons = sum(CO2..short.tons., na.rm = TRUE))

Line_graph = ggplot(data = RRGI_Line, aes(x = paste(Year, Quarter, sep = 'Q'), y = CO2.tons/1e6, group = 1)) +
  geom_line(colour = 'orange', size = 1) + 
  ylim(0,50) + 
  xlab(NULL) + ylab('CO2 Emissions (Million Short Tons)') +
  ggtitle("Total CO2 Emissions by Regional Greenhouse Gas Initiative States") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Line_graph

RRGI_Bar = filter(RRGI_Summaries, State != 'NJ', Year != '2016') %>%
  group_by(Year, Source.Category) %>% 
  summarise(facilities = n())

Bar_graph = ggplot(data = RRGI_Bar, aes(x = Year, y = facilities, fill = Source.Category)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) + ylab('Facilities') +
  ggtitle("Facility Count by Source Category") +
  scale_fill_brewer(direction = -1) + theme_dark()

Bar_graph
#Group into Pre2014, and post2014

test2014 = mutate(RRGIdata, status = ifelse(Year < 2014, 'Before', 'After'))

#F test to compare variance
sampbefore = filter(test2014, status == 'Before')$CO2..short.tons.
sampafter = filter(test2014, status == 'After')$CO2..short.tons.
var.test(sampbefore,sampafter)
#p value < 2.2e-16, the two variances are not homogenous
#two sample T test
ttest2014 = t.test(sampbefore,sampafter,var.equal = FALSE, paired = FALSE)
#p value < .05, therefore we reject the null hypothesis that the means are equal. There has been a change.

ttestplot = ggplot(data = test2014, aes(x = CO2..short.tons.)) + 
  geom_histogram(binwidth = 200) +
  facet_grid(status ~ .) +
  xlim(0, 1e+05) + 
  ggtitle('Effectiveness of 2014 Regulations') +
  geom_text(aes(75000,600, label = 'mean.before = 60,376.53\nmean.after = 54,963.20\n2 sample T-test p = 0.01145'))