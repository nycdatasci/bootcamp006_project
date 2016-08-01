library(ggplot2)
library(plyr)
library(dplyr)

setwd('~/Desktop/DataScienceAcademy/Projects/Visualization with ggplot2')

national = read.csv('NationalNames.csv')
state = read.csv('StateNames.csv')

str(national)
head(national)
str(state)
head(state)
# str(births)

range(national$Year)
length(unique(national$Year))
# year goes from 1880 to 2014
# total of 135 years

start_year = 1914
end_year = 2014
selected_years = seq(start_year, end_year, 1)
num_levels = (end_year - start_year)/10



#================================find number of unique baby names
national_year = national %>% group_by(Year) %>% summarise(count=sum(Count))
range(national_year$count)

filtered_national = national %>% filter(Year>=start_year & Year<=end_year)
filtered_national$bins = cut(filtered_national$Year, breaks=num_levels)

distinct_national = filtered_national %>% group_by(Year) %>% summarise(Count = n_distinct(Name))
distinct_range = max(distinct_national$Count) - min(distinct_national$Count)
distinct_national$Normalized = (distinct_national$Count - min(distinct_national$Count)) / distinct_range


ggplot(distinct_national, aes(Year, Count)) + 
  geom_line(color='red') + 
  xlim(start_year, end_year) + 
  ylim(0, round_any(max(distinct_national$Count), 10000, f=ceiling)) +
  ggtitle('Number of unique baby names')


#================================plot against population 
population = read.csv('population.csv', header=T)
head(population)

population = population %>% filter(Year>=start_year & Year<=end_year)

ggplot(population, aes(Year, Change/1e6)) + 
  geom_line(color='blue') + 
  ylab('Population (in millions)') + 
  ggtitle('U.S. population')
    

population_range = max(population$Change) - min(population$Change)
population$Normalized = (population$Change - min(population$Change)) / population_range
population$distinct_national = distinct_national$Normalized

ggplot(population, aes(Year)) + 
  geom_line(aes(y=distinct_national, color='blue')) +
  geom_line(aes(y=Normalized, color='red')) +
  scale_colour_manual("",
                   labels = c("Unique names", "Population"),
                   values = c("red", "blue")) +
  ggtitle('Number of unique names against population')

#================================population per name

population_per_name = data.frame(Year = selected_years)
population_per_name$Ratio = population$Change / (filtered_national %>% group_by(Year) %>% summarise(Count = n_distinct(Name)))$Count

ggplot() +
  geom_line(data=population_per_name, aes(Year, Ratio),color='purple') +
  ylab('Population per name (in thousands)') +
  ggtitle('Population per name') + 
  ylim(0, round_any(max(population_per_name$Ratio), 10000, f=ceiling))

(max(population_per_name$Ratio) - min(population_per_name$Ratio)) / max(population_per_name$Ratio)
  

#================================ sample infrequent names

infrequent_names = sort(unique(filtered_national$Name[which(is.na(match(filtered_national$Name, filtered_state$Name)))]))

length(infrequent_names) / nrow(filtered_national)


#================================ average length of name
average_length = filtered_national %>% group_by(Year) %>% summarise(Length = mean(nchar(unique(as.character(Name)))))

ggplot(average_length, aes(Year, Length)) + 
  geom_step() + 
  ylim(4,8) + 
  ggtitle('Average length of baby names')
  

#================================ number of gender-neutral names

neutral_over_year = data.frame(Year=selected_years, Count=rep(NA, length(selected_years)))

for (i in 1:length(selected_years)) {
  temp_f = filtered_national %>% filter(Year==as.character(selected_years[i])) %>% filter(Gender=='F') %>% select(Name, Count) 
  temp_m = filtered_national %>% filter(Year==as.character(selected_years[i])) %>% filter(Gender=='M') %>% select(Name, Count) 
  
  neutral_over_year[i,2] = sum(is.na(match(temp_f$Name, temp_m$Name))=='FALSE')
}


ggplot(neutral_over_year, aes(Year, Count)) + 
  geom_line() + 
  xlim(start_year, end_year) + 
  ylim(0, round_any(max(neutral_over_year$Count), 1000, f=ceiling)) +
  ggtitle('Number of gender-neutral baby names') 








