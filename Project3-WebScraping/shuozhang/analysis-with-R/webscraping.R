setwd("~/Desktop/web scraping")
library(dplyr)
library(dygraphs)
library(reshape2)
library(ggplot2)
library(plotly)
library(car)
library(TSA)
library(forecast)
library(xts)
library(Rmisc)
library(NbClust)
library(cluster)
library(factoextra)
library(ggrepel)
output=read.csv('output.txt', header = T, stringsAsFactors = F)
output2012=filter(output, year==2012)
summary(output)
sapply(output[,c('Gold', 'Silver', 'Bronze', 'Total')], sd)

# pick the top countries by gold and total
output_bycountry=output%>%
  dplyr::group_by(Country)%>%
  summarise(totoalgold=sum(Gold), avggold=mean(Gold))%>%
  arrange(desc(avggold))
head(output_bycountry,20)
# Country totoalgold   avggold
# <chr>      <int>     <dbl>
#   1   Unified Team         45 45.000000
# 2   Soviet Union        395 43.888889
# 3  United States        990 36.666667 (1)
# 4   East Germany        153 30.600000
# 5          China        201 25.125000 (2)
# 6         Russia        134 19.142857 (3)
# 7        Germany        213 11.210526 (4)
# 8   West Germany         56 11.200000
# 9  Great Britain        245  8.750000 (5)
# 10         Italy        211  8.115385 (6)
# 11        France        222  7.928571 (7)
# 12       Ukraine         33  6.600000
# 13       Hungary        170  6.538462 (8)
# 14         Japan        130  6.500000 (9)
# 15        Sweden        144  5.538462 (10)
# 16   South Korea         81  5.400000 (11)
# 17     Australia        138  5.307692 (12)
# 18          Cuba         71  5.071429
# 19       Romania         88  4.888889
# 20       Finland        106  4.240000
output_bycountry2=output%>%
  dplyr::group_by(Country)%>%
  summarise(totoalmetal=sum(Total), avgmetal=mean(Total))%>%
  arrange(desc(avgmetal))
head(output_bycountry2,20)
# Country totoalmetal  avgmetal
# <chr>       <int>     <dbl>
#   1   Soviet Union        1010 112.22222
# 2   Unified Team         112 112.00000
# 3  United States        2425  89.81481 (1)
# 4   East Germany         409  81.80000
# 5          China         473  59.12500  (2)
# 6         Russia         405  57.85714 (3)
# 7   West Germany         204  40.80000
# 8        Germany         728  38.31579 (4)
# 9  Great Britain         811  28.96429 (5)
# 10        France         727  25.96429 (6)
# 11       Ukraine         115  23.00000
# 12         Italy         577  22.19231 (7)
# 13         Japan         400  20.00000 (8)
# 14        Sweden         498  19.15385 (9)
# 15       Hungary         490  18.84615 (10)
# 16     Australia         475  18.26923 (11)
# 17       Romania         301  16.72222
# 18   South Korea         243  16.20000 (12)
# 19       Belarus          75  15.00000
# 20          Cuba         202  14.42857

# pick the top countries
top10=dplyr::filter(output, Country %in% c('United States','China', 'Great Britain',
                                          'Russia', 'South Korea', 'Germany',
                                          'France', 'Italy', 'Hungary', 
                                          'Japan', 'Australia', 'Brazil', 'Sweden'))

# plot dygraphs
df2=dcast(top10[,c('Country', 'Total', 'year')], year~ Country,value.var="Total")
color=c('#dddfd4', '#fae596', '#3fb0ac', '#173e43', '#98dafc', '#e62739', '#9068be',
        '#dbc3d0', '#6a5750', '#e05915', '#bccbde', '#300032', '#6534ff')
dygraph(df2) %>%
  dyOptions(colors = color) %>%
  dyRangeSelector()

# ggplot of  boxplot
ggplot(data=top10, aes(x=reorder(Country,Total), y=Total, color=Country))+
  geom_boxplot()+  
  xlab('Country')+
  ylab('Number of total medals')+
  ggtitle('Medal distribution by country')+
  theme_bw()+
  theme(legend.position='none', axis.text.x = element_text(angle = 45, hjust = 1))

# population
population=read.csv('population_both_gender.csv', header = T, stringsAsFactors = F)
summary(population)

population1_2012=population %>%
  dplyr::filter(Place %in% output2012$Country)%>%
  select(Country=Place, Population=X2012)
#nrow(population1_2012)
population2_2012=population %>%
  dplyr::filter(Place %in%          c('China, Hong Kong SAR',
                                    'United Kingdom',
                                    'Iran (Islamic Republic of)',
                                    'Republic of Moldova',
                                    "Dem. People's Republic of Korea",
                                    'Russian Federation',
                                    'Republic of Korea',
                                    'United States of America',
                                    'Venezuela (Bolivarian Republic of)')) %>%
  select(Country=Place, Population=X2012)
population2_2012$Country=c('Hong Kong','Great Britain',
                           'Iran', 'Moldova', 'North Korea',
                           'Russia', 'South Korea', 'United States',
                           'Venezuela')
population_2012=rbind(population1_2012, population2_2012)
taibei=cbind(Country=c('Chinese Taipei'), Population=as.numeric(23234.940))
population_2012=rbind(population_2012, taibei)
#str(population_2012)
output2012_bypopulation=merge(x=output2012, y=population_2012, by='Country', all.x=T)


#GDP
GDP=read.csv('GDP.csv', header = T, stringsAsFactors = F)
#View(GDP)
summary(GDP)

GDP1_2012=GDP %>%
  dplyr::filter(Country.Name %in% output2012$Country)%>%
  select(Country=Country.Name, GDP=X2012)
#nrow(GDP1_2012)

merge(x=output2012, y=GDP1_2012, by='Country', all.x=T)
addition=c('Bahamas, The', 'Egypt, Arab Rep.', 'United Kingdom', 'Hong Kong SAR, China',
  'Iran, Islamic Rep.', 'Korea, Dem. People’s Rep.', 'Russian Federation',
  'Slovak Republic', 'Korea, Rep.', 'Venezuela, RB')
GDP2_2012=GDP %>%
  dplyr::filter(Country.Name %in% addition)%>%
  select(Country=Country.Name, GDP=X2012)

GDP2_2012$Country=c('Bahamas','Egypt','Great Britain', 'Hong Kong',
  'Iran', 'North Korea', 'Russia', 'Slovakia', 'South Korea', 'Venezuela')
GDP_2012=rbind(GDP1_2012, GDP2_2012)
taipei=cbind(Country=c('Chinese Taipei'), GDP=as.numeric(42201.09))	
GDP_2012=rbind(GDP_2012, taipei)
output2012_bypopulationGDP=merge(x=output2012_bypopulation, y=GDP_2012, by='Country', all.x=T)
output2012_bypopulationGDP[is.na(output2012_bypopulationGDP)]=15155.2
#sum(is.na(output2012_bypopulationGDP))#none na
#str(output2012_bypopulationGDP)
output2012_bypopulationGDP['Population']=as.numeric(output2012_bypopulationGDP$Population)
output2012_bypopulationGDP['GDP']=as.numeric(output2012_bypopulationGDP$GDP)

#GDP growth
GDPgrowth=read.csv('GDPgrowth.csv', header = T, stringsAsFactors = F)
#summary(GDPgrowth)
GDPgrowth1_2012=GDPgrowth %>%
  dplyr::filter(Country.Name %in% output2012$Country)%>%
  select(Country=Country.Name, GDPgrowth=X2012)
#nrow(GDP1_2012)
addition=c('Bahamas, The', 'Egypt, Arab Rep.', 'United Kingdom', 'Hong Kong SAR, China',
           'Iran, Islamic Rep.', 'Korea, Dem. People’s Rep.', 'Russian Federation',
           'Slovak Republic', 'Korea, Rep.', 'Venezuela, RB')
GDPgrowth2_2012=GDPgrowth %>%
  dplyr::filter(Country.Name %in% addition)%>%
  select(Country=Country.Name, GDPgrowth=X2012)

GDPgrowth2_2012$Country=c('Bahamas','Egypt','Great Britain', 'Hong Kong',
                    'Iran', 'North Korea', 'Russia', 'Slovakia', 'South Korea', 'Venezuela')
GDPgrowth_2012=rbind(GDPgrowth1_2012, GDPgrowth2_2012)
taipei1=cbind(Country=c('Chinese Taipei'), GDPgrowth=as.numeric(2.06))	
GDPgrowth_2012=rbind(GDPgrowth_2012, taipei1)
output2012_bypopulationGDP1=merge(x=output2012_bypopulationGDP, y=GDPgrowth_2012, by='Country', all.x=T)
#str(output2012_bypopulationGDP1)
#sum(is.na(output2012_bypopulationGDP1))
output2012_bypopulationGDP1[is.na(output2012_bypopulationGDP1)]=3.4
output2012_bypopulationGDP1$GDPgrowth=as.numeric(output2012_bypopulationGDP1$GDPgrowth)

#life expectency
life=read.csv('life.csv', header = T, stringsAsFactors = F)
#summary(life)
trim.leading <- function (x)  sub("^\\s+", "", x)
life$Country=trim.leading(life$Country)
life_2012=life %>%
  dplyr::filter(Country %in% output2012$Country)%>%
  select(Country, Life)
merge(x=output2012_bypopulationGDP1,y=life_2012, by='Country', all.x=T)%>%
  select(Country, Life)
#Bahamas, Chinese Taipei,Great Britain, Montenegro
addition=c('The Bahamas', 'United Kingdom')
life1_2012=life %>%
  dplyr::filter(Country %in% addition)%>%
  select(Country, Life)
life1_2012$Country=c('Bahamas', 'Great Britain')
Taibei=cbind(Country=c('Chinese Taipei', 'Montenegro'), 
             Life=as.numeric(79.5, 74.65))
life2_2012=rbind(life_2012,life1_2012,Taibei)
output2012_bypopulationGDP2=merge(x=output2012_bypopulationGDP1,
                                  y=life2_2012, by='Country', all.x=T)
#str(output2012_bypopulationGDP2)
output2012_bypopulationGDP2$Life=as.numeric(output2012_bypopulationGDP2$Life)
summary(output2012_bypopulationGDP2)

# kmeans model
df=select(output2012_bypopulationGDP2,Country,Total,Population,GDP, GDPgrowth, Life)
df$Population=df$Population/1000
df$GDP=df$GDP/1000
plot(df$Total, df$GDP, col='red', xlab="Total medals", ylab='')
points(df$Total,df$GDPgrowth,col='blue')
points(df$Total,df$Population,col='orange')
points(df$Total, df$Life, col='green')
legend("topright", inset=.05, 
       c("GDP","GDP growth","Population", 'Life expectancy'),
       pch=c(10,10, 10, 10),
       col=c('red','blue', 'orange', 'green' ), horiz=FALSE)


ddf=scale(df[,-1])
set.seed(1234)
fviz_nbclust(ddf, kmeans, method = "silhouette")
nc <- NbClust(ddf, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria")
set.seed(1234)
fit.km <- kmeans(ddf, 4, nstart=25)  
fviz_cluster(fit.km, data = ddf, geom = "point",
             stand = FALSE, frame.type = "norm")
fit.km$size
fit.km$centers
aggregate(df[,-1], by=list(cluster=fit.km$cluster), mean)

#str(df)
df$cluster <- as.factor(fit.km$cluster)
plot_ly(df, x = Total, y = Population, z= GDP, text = paste("Country: ", Country),
        type="scatter3d", mode="markers", color=cluster)
plot_ly(df, x = Total, y = Life, z= GDPgrowth, text = paste("Country: ", Country),
        type="scatter3d", mode="markers", color=cluster)


df1=df%>%
  dplyr::filter(cluster==1)%>%
  arrange(desc(Total))

df2=df%>%
  dplyr::filter(cluster==2)%>%
  arrange(desc(Total))

df3=df%>%
  dplyr::filter(cluster==3)%>%
  arrange(desc(Total))

df4=df%>%
  dplyr::filter(cluster==4)%>%
  arrange(desc(Total))

df8=rbind(df1, filter(df2, Country%in%c('South Korea',
                                        'Hungary',
                                        'Brazil',
                                        'Mexico',
                                        'Greece',
                                        'Portugal')),
          head(df3,6),
          filter(df4, Country%in%c('United States', 'Russia',
                                   'Great Britain', 'Japan',
                                   'Australia',
                                   'France',
                                   'New Zealand',
                                   'Sweden','Chinese Taipei', 'Hong Kong')))
        
#str(df8)
g1=ggplot(df8, aes(Total,Population, color =cluster)) +
  geom_point()+
  geom_text_repel(aes(label=Country))+
  theme_classic()+
  theme_bw()
g2=ggplot(df8, aes(Total,GDP, color =cluster)) +
  geom_point()+
  geom_text_repel(aes(label=Country))+
  theme_classic()+
  theme_bw()
g7=ggplot(df8, aes(Total,GDPgrowth, color =cluster)) +
  geom_point()+
  geom_text_repel(aes(label=Country))+
  theme_classic()+
  theme_bw()
g9=ggplot(df8, aes(Total,Life, color =cluster)) +
  geom_point()+
  geom_text_repel(aes(label=Country))+
  theme_classic()+
  theme_bw()

multiplot(g1,g2, g7, g9,cols = 2)

g3=ggplot(df, aes(x=reorder(cluster, Total), y=Total, color=cluster))+
  geom_point()+
  geom_boxplot()+
  xlab('Cluster')+
  theme_bw()
g4=ggplot(df, aes(x=reorder(cluster, Population), y=Population, color=cluster))+
  geom_point()+
  geom_boxplot()+
  xlab('Cluster')+
  theme_bw()
g5=ggplot(df, aes(x=reorder(cluster, GDP), y=GDP, color=cluster))+
  geom_point()+
  geom_boxplot()+
  xlab('Cluster')+
  theme_bw()
g8=ggplot(df, aes(x=reorder(cluster, GDPgrowth), y=GDPgrowth, color=cluster))+
  geom_point()+
  geom_boxplot()+
  xlab('Cluster')+
  theme_bw()
g10=ggplot(df, aes(x=reorder(cluster, Life), y=Life, color=cluster))+
  geom_point()+
  geom_boxplot()+
  xlab('Cluster')+
  theme_bw()
multiplot(g3,g4,g5,g8,g10, cols=2)



