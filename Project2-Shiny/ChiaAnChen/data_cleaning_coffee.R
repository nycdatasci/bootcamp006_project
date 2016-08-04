# NYC Data Science Academy
# Project 2: Shiny App
# Author: Chia-An Chen
# 07/31/16

# Data Source: 
# production, supply and distribtion of coffee_df: https://apps.fas.usda.gov/psdonline/psdDownload.aspx
# price of coffee_df, and price paid to farmers: http://www.ico.org/new_historical.asp
# Date downloaded: 07/22/2016

############################################################
## Clean the dataset to create coffee_master_df for app.R ##
############################################################
# Load libraries
# library(ggplot2)
# library(cowplot)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(scales)
library(googleVis)
library(maps)


# Load files
# Modify the path accordindly if you download the data to your computer
# production, supply, and distribution of coffee_dffrom 1959 to 2016
coffee = read.csv("/Users/annecool37/Dropbox/DataScience/Project2/dataset/psd_coffee.csv", header = TRUE, sep = ",")
# population from 1966 to 2015
population = read.csv("/Users/annecool37/Dropbox/DataScience/Project2/dataset/population.csv", header = TRUE, sep = ",")[1:218,]
# annual working hour
working_hr = read.csv("/Users/annecool37/Dropbox/DataScience/Project2/dataset/annual_working_hour.csv", header = TRUE, sep = ",") 
working_hr = select(working_hr, 2, 8, 15)
# gdp
gdp = read.csv("/Users/annecool37/Dropbox/DataScience/Project2/dataset/gdp.csv", header = TRUE, sep = ",")[1:218,]
names(gdp) = c("Series.Name","Series.Code","Country.Name","Country.Code", seq(1966, 2015))


# # Manually Identify countries names that labeled differently in map
# map = map_data("world")
# countries_names_in_map = as.vector(summarise(group_by(map, region))) %>% mutate(count1 =1)
# countries_names_in_coffee_df = group_by(coffee_df, region) %>% summarise() %>% mutate(count2 =1)
# countries_names_in_population = group_by(population, Country.Name) %>% summarise() %>% transmute(region = as.character(Country.Name), count2 =1)
# countries_names_in_price_paid = group_by(price_paid, region) %>% summarise() %>% mutate(count2 =1)
# countries_names_in_gdp = group_by(gdp, Country.Name) %>% summarise() %>% mutate(count2 =1)
# binded_countries = full_join(countries_names_in_map, countries_names_in_coffee_df)
# binded_countries = full_join(countries_names_in_map, countries_names_in_population)
# binded_countries = full_join(countries_names_in_map, countries_names_in_price_paid)
# binded_countries = full_join(countries_names_in_map, countries_names_in_gdp)
# View(binded_countries)

# Reassign country name in population to match with world map in maps packages
misuse_names = c("Congo, Dem. Rep.", "Congo, Rep.", "Cote d'Ivoire", "Egypt, Arab Rep.", "Iran, Islamic Rep.", "Korea, Rep.","Lao PDR","Russian Federation", "United Kingdom", "United States", "Venezuela, RB")       
names_in_map = c("Democratic Republic of the Congo", "Republic of Congo", "Ivory Coast", "Egypt", "Iran", "South Korea", "Laos", "Russia", "UK", "USA", "Venezuela")
population_df = mutate(population, region = as.character(Country.Name))
for (i in (1:length(misuse_names))) {
    population_df$region[population_df$region == misuse_names[i]] = names_in_map[i]
}
# rearrange population new column "year" and "count"
population_df =  population_df[5:55]
names(population_df) = c(as.character(seq(1966, 2015)), "region")
population_df= gather(population_df, key = region, value = "year")
names(population_df) = c("region","year","count")

# Reassign country name in coffee_df to match with world map in maps packages
misuse_names = c("Congo (Brazzaville)", "Congo (Kinshasa)", "Cote d'Ivoire", "Korea, South", "United States", "Yemen (Sanaa)" )
names_in_map = c("Republic of Congo", "Democratic Republic of the Congo", "Ivory Coast", "South Korea", "USA", "Yemen")
coffee_df = mutate(coffee, "region" = as.character(Country_Name), "kg" = Value*1000*60, "year" = Market_Year)
for (i in (1:length(misuse_names))) {
    coffee_df$region[coffee_df$region == misuse_names[i]] = names_in_map[i]
}

# Reassign country name in price_paid to match with world map in maps packages
misuse_names = c("C_te d'Ivoire", "Congo, Dem. Rep. of", "Congo, Rep. of", "Trinidad & Tobago", "Venezuela, Bol. Rep. of", "Zimbabwe ")       
names_in_map = c("Ivory Coast", "Democratic Republic of the Congo", "Republic of Congo",  "Trinidad and Tobago", "Venezuela", "Zimbabwe")
price_paid = mutate(price_paid, region = as.character(region))
for (i in (1:length(misuse_names))) {
    price_paid$region[price_paid$region == misuse_names[i]] = names_in_map[i]
}

# Reassign country name in gdp to match with world map in maps packages
misuse_names = c("Congo, Dem. Rep.", "Congo, Rep.", "Cote d'Ivoire", "Egypt, Arab Rep.", "Iran, Islamic Rep.", "Korea, Rep.","Lao PDR","Russian Federation", "United Kingdom", "United States", "Venezuela, RB", "Yemen, Rep.")       
names_in_map = c("Democratic Republic of the Congo", "Republic of Congo", "Ivory Coast", "Egypt", "Iran", "South Korea", "Laos", "Russia", "UK", "USA", "Venezuela", "Yemen")
gdp_df = mutate(gdp, region = as.character(Country.Name))
for (i in (1:length(misuse_names))) {
    gdp_df$region[gdp_df$region == misuse_names[i]] = names_in_map[i]
}
gdp_df = gdp_df [5:55]
gdp_df = gather(gdp_df, key = region, value = "year")
gdp_df$year[gdp_df$year == ".."] = NA
gdp_df$year[gdp_df$year == 0 ] = NA
gdp_df = gdp_df[complete.cases(gdp_df),]
names(gdp_df) = c("region","year","usd")
gdp_df = mutate(gdp_df, "year" = as.numeric(year), "usd" = as.numeric(usd)) %>%
    rename("gdp_usd" = usd)

# Reassign country name in working_hr to match with world map in maps packages
misuse_names = c("United States", "Korea")       
names_in_map = c("USA", "South Korea")
working_hr_df = rename(working_hr, "region" = Country, "year" = Time, "count" = Value) %>%
  mutate("region" = as.character(region))
for (i in (1:length(misuse_names))) {
  working_hr_df$region[working_hr_df$region == misuse_names[i]] = names_in_map[i]
}
View(working_hr_df)

# reformat price_paid
price_paid_df = gather(price_paid[2:28], region, "year")
names(price_paid_df) = c("region", "year", "price_paid")
price_paid_df = full_join(price_paid_df, price_paid[1:2], by = "region") %>%
  mutate("year" = as.numeric(year))

# create european rows for population_df and working_hr_df
eu_names = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
             "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
             "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", 
             "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "UK")

# a function that sum data for countries in EU 
# and create a region EU to match the coffee dataset
add_eu = function(df){
  eu_df = filter(df, df$region %in% eu_names) %>%
    group_by(year) %>% summarise("count" = sum(as.numeric(count))) 
  eu_df$region = "European Union"
  updated_df = rbind(df, eu_df) %>% 
    mutate("year" = as.numeric(year), "count" = as.numeric(count)) 
  return (updated_df)
}

# update population and working hour dataframe by adding "european union" as a whole to original dataframe
updated_population_df = add_eu(population_df) %>% rename("num_person" = count)
updated_working_df = add_eu(working_hr_df) %>% rename("working_hr" = count)

# manipulate european countries
# clean coffee and filter out data in year after 2015 
# 2016 is the market year for coffee, however, populationa and gdp data are not updated yet
coffee_tidy_df = select(coffee_df, 9,13,14,15) %>% filter(year < 2016)
euro_coffee = filter(coffee_tidy_df, region == "European Union")

# create a master dataframe for app.R
coffee_master_df = left_join(coffee_tidy_df, gdp_df) %>% left_join(updated_population_df) %>% 
  left_join(updated_working_df) %>% left_join(price_paid_df) %>% 
  mutate("kg_per_gdp" = kg/gdp_usd, "kg_per_person" = kg/num_person)
coffee_master_df = unique(coffee_master_df)

# write the master dataframe to coffee_master_df.csv
write.csv(coffee_master_df, file = "/Users/annecool37/Dropbox/DataScience/Project2/coffee_app/data/coffee_master_df.csv")

######## Dataset not being implemented ########
# price paid to growers, US cents/lb 
# mildtype was manually added accordingly via excel
# price_paid = read.csv("/Users/annecool37/Dropbox/DataScience/Project2/dataset/prices_paid_to_growers.csv", header = TRUE, sep = ",")[2:65,]
# names(price_paid) = c("mildtype", "region", seq(1990, 2015))
