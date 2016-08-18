setwd("/Users/williambartlett/PycharmProjects/Web_scraping_project/Rfiles")
library(readxl)
library(dplyr)
library(ggplot2)
library(stats)
library(stringr)
library(tidyr)
library(wordcloud)
library(ggthemes)
load("info_df_marktags.RData")

locations_df = select(info_df_marktags, Company, Location, num_locations)

locations_df = separate(data = locations_df, 
                   col = Location, 
                   into = c("Location_1", "Location_2"), 
                   sep = " · ")
to_stack = locations_df[,c(2,3)]
stacked = stack(to_stack)
stacked = stacked[complete.cases(stacked),]
sum(locations_df$num_locations, na.rm = T)

loc_count = count(stacked, values)
loc_count = arrange(loc_count, desc(n))
top_loc = loc_count[c(1:25),]
top_loc$values = factor(top_loc$values, levels = top_loc$values[order(top_loc$n)])

g = ggplot(top_loc, mapping = aes(x = values, y = n))
bar = g + geom_bar(stat = "identity", fill = "yellow") + 
      coord_flip() + 
      labs(title = "Top 25 Most Frequent Locations of Health Care Start-ups on Angelist.com", x = "Location", y = "Count") + 
      theme_economist() +
      scale_y_continuous(limits = c(0,200))
bar

top_loc_sf = top_loc

top_loc_sf$values = gsub("Palo Alto", "Bay Area", top_loc_sf$values)
top_loc_sf$values = gsub("Mountain View", "Bay Area", top_loc_sf$values)
top_loc_sf$values = gsub("Menlo Park", "Bay Area", top_loc_sf$values)
top_loc_sf$values = gsub("Silicon Valley", "Bay Area", top_loc_sf$values)
top_loc_sf$values = gsub("San Mateo", "Bay Area", top_loc_sf$values)
top_loc_sf$values = gsub("San Francisco", "Bay Area", top_loc_sf$values)
top_loc_sf$values = gsub("Redwood City", "Bay Area", top_loc_sf$values)
top_loc_sf$values = gsub("San Carlos", "Bay Area", top_loc_sf$values)
top_loc_sf$values = gsub("Oakland", "Bay Area", top_loc_sf$values)
top_loc_sf$values = gsub("Brooklyn", "New York City", top_loc_sf$values)
top_loc_sf = top_loc_sf%>%
      group_by(values)%>%
      summarise(Count = sum(n))%>%
      arrange(desc(Count))

top_loc_sf$values = factor(top_loc_sf$values, levels = top_loc_sf$values[order(top_loc_sf$Count)])

g_sf = ggplot(top_loc_sf, mapping = aes(x = values, y = Count))
bar_sf = g_sf + geom_bar(stat = "identity", fill = "yellow") + 
      coord_flip() + 
      labs(title = "Combined Location Frequencies", x = "Location", y = "Count") + 
      theme_economist() 
bar_sf




