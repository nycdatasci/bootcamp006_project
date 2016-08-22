setwd("/Users/williambartlett/PycharmProjects/Web_scraping_project/Rfiles")
load('activity_table.RData')
load('Founders_dict.Rdata')
library(reshape) 
library(reshape2)
library(readxl)
library(dplyr)
library(ggplot2)
library(stats)
library(stringr)
library(tidyr)
library(ggthemes)

#____FOUNDERS DATA MANIP_______________________
load('Founders_dict.Rdata')
colnames(founders_dict) = c("Company", "Founders")
founders_dict$Founders = gsub("[u'", "", founders_dict$Founders, fixed = T)
founders_dict$Founders = gsub("u'", "", founders_dict$Founders, fixed = T)
founders_dict$Founders = gsub("[[:punct:]]", "", founders_dict$Founders, fixed = F)
founders_dict$Founders = gsub("^\\s+|\\s+$", "", founders_dict$Founders, fixed = F)





#_____NEW DATA MANIP___________________________

new = activity

# new$Activity = gsub('[0-9]+', "", new$Activity)
# new$Activity = gsub('[[:punct:]]', "", new$Activity)
# new$Activity = gsub('Sep', "TTT", new$Activity)
# new$Activity = gsub('Sep', "TTT", new$Activity)
# new$Activity = gsub('Jan', "TTT", new$Activity)
# new$Activity = gsub('Feb', "TTT", new$Activity)
# new$Activity = gsub('Mar', "TTT", new$Activity)
# new$Activity = gsub('Apr', "TTT", new$Activity)
# new$Activity = gsub('May', "TTT", new$Activity)
# new$Activity = gsub('Jun', "TTT", new$Activity)
# new$Activity = gsub('Jul', "TTT", new$Activity)
# new$Activity = gsub('Aug', "TTT", new$Activity)
# new$Activity = gsub('Oct', "TTT", new$Activity)
# new$Activity = gsub('Nov', "TTT", new$Activity)
# new$Activity = gsub('Dec', "TTT", new$Activity)
# new$Activity = gsub('INVESTOR PASSED', "", new$Activity)
# View(new)

new = activity[,-2]
new = as.data.frame(new)
new = melt(new, id = 'Company')
new = new[complete.cases(new),]
new = separate(data = new, 
               col = value,
               into = c("Investor", "Date"),
               sep = ':')
new$Date = gsub('INVESTOR PASSED', "", new$Date)
new = filter(new, Investor != "INVESTOR PASSED")
new$Investor = gsub("https", NA, new$Investor)
new = new[complete.cases(new),]
new$Date = gsub('[[:punct:]]', '', new$Date)
new$Date = gsub("^\\s+|\\s+$", "", new$Date)

new$Date = strptime(x = new$Date, format = '%b %e %Y')
new$Date = as.POSIXct(new$Date)
new = new[complete.cases(new),]



#___________________Summary Table_________________
fm = 1.051e7
m = fm/4
employee_nums = info_df_marktags[,c(1,5)]

sum_table = new %>%
      arrange(Date)%>%
      group_by(Company) %>%
      summarise(Founding_date = first(Date),
                First_actor = first(Investor),
                four_mo_invests = sum(Date<Founding_date+fm),
                eight_mo_invests_inc = sum(Date<Founding_date+2*fm),
                eight_mo_invests_n_inc = sum(Date<Founding_date+2*fm &
                                                   Date>Founding_date+fm),
                year_invests_inc = sum(Date<Founding_date+3*fm),
                year_invests_n_inc = sum(Date<Founding_date+3*fm &
                                               Date>Founding_date+2*fm) )%>%
      left_join(founders_dict, by = "Company")%>%
      left_join(employee_nums, by = "Company")
sum_table$First_actor = gsub("^\\s+|\\s+$", "", sum_table$First_actor)
sum_table$Founders = gsub("^\\s+|\\s+$", "", sum_table$Founders)

sum_table = sum_table %>%
      mutate(First_Actor_founder = First_actor %in% Founders)%>%
      mutate(Age_days = as.POSIXlt(Sys.Date()) - Founding_date)%>%
      mutate(Age_months = Age_days/30)

sum_table = separate(sum_table, col = Employee_num_Range, c("emp_low", "emp_high"), sep = '-')
sum_table$emp_low = gsub("^\\s+|\\s+$", "", sum_table$emp_low)
sum_table$emp_high = gsub("^\\s+|\\s+$", "", sum_table$emp_high) 
sum_table$emp_low = as.numeric(sum_table$emp_low)
sum_table$emp_high = as.numeric(sum_table$emp_high)
sum_table = mutate(sum_table, emp_avg = (emp_low+emp_high)/2)%>%
      mutate(emp_per_month = emp_avg/as.numeric(Age_months))

load("top_table.RData")

info_df_marktags$top_tag = NA
info_df_marktags$`Market Tag 1` = gsub("^\\s+|\\s+$", "", info_df_marktags$`Market Tag 1`)
info_df_marktags$`Market Tag 2` = gsub("^\\s+|\\s+$", "", info_df_marktags$`Market Tag 2`)
info_df_marktags$`Market Tag 3` = gsub("^\\s+|\\s+$", "", info_df_marktags$`Market Tag 3`)
info_df_marktags$`Market Tag 4` = gsub("^\\s+|\\s+$", "", info_df_marktags$`Market Tag 4`)

top_table = top_table[-c(27,28),]

top_tag = c()
for(i in 1:nrow(info_df_marktags)){
      top_tag[i] = intersect(top_table$tag, c(info_df_marktags$`Market Tag 1`[i],
                            info_df_marktags$`Market Tag 2`[i],
                            info_df_marktags$`Market Tag 3`[i],
                            info_df_marktags$`Market Tag 4`[i]))[1]
}
info_df_marktags$top_tag = top_tag    
top_tag_merg = info_df_marktags[,c(1,14)]
sum_table = left_join(sum_table, top_tag_merg, by = "Company")
sum_table$top_tag = gsub("Big Data Analytics", "Big Data", sum_table$top_tag)
sum_table$top_tag = as.factor(sum_table$top_tag)
levels(sum_table$top_tag)
info_df_marktags = separate(data = info_df_marktags,
                        col = Location, into = c('loc_1', 'loc_2'),
                        sep = " · ")
info_df_marktags$loc_1 = gsub("^\\s+|\\s+$", "", info_df_marktags$loc_1)
info_df_marktags$loc_2 = gsub("^\\s+|\\s+$", "", info_df_marktags$loc_2)
sum_table = inner_join(sum_table, info_df_marktags[,c(1,3)], by = "Company")
levels(as.factor(sum_table$loc_1))


#__________INVESTMENTS BY LOCATION__________
location_stats = sum_table%>%
      group_by(loc_1)%>%
      summarise(avg_invests = mean(year_invests_inc),
                count = n())%>%
      filter(count>5)%>%
      arrange(desc(count))
#______graph____
location_stats$loc_1 = factor(location_stats$loc_1, levels = location_stats$loc_1[order(location_stats$count)])

loc = ggplot(location_stats, mapping = aes(loc_1, avg_invests))
bar_g = loc + geom_bar(stat = "identity", fill = "yellow") + 
      coord_flip() + 
      labs(title = "Average Number of First Year Activity Entries (Investors) by Location", x = "Location", y = "Avg_Entries") + 
      theme_economist() 
bar_g

model = lm(data = sum_table, formula = emp_per_month ~ top_tag)
model = aov(data = sum_table, formula = year_invests_inc ~ top_tag)
model
summary(model)
plot(model)


#______INVESTMENTS BY MARKET TAGS____________

tag_stats = sum_table%>%
      filter(is.na(top_tag) == F)%>%
      group_by(top_tag)%>%
      summarise(avg_invests = mean(year_invests_inc),
                avg_growth_rate = mean(emp_per_month),
                count = n())
   

#______graph____

tag_stats$top_tag = factor(tag_stats$top_tag, levels = tag_stats$top_tag[order(tag_stats$avg_invests)])

tag = ggplot(tag_stats, mapping = aes(top_tag, avg_invests))
bar_tag = tag + geom_bar(stat = "identity", fill = "yellow") + 
      coord_flip() + 
      labs(title = "Average Number of First Year Activity Entries (Investors) by Top Market Tag", x = "Location", y = "Avg_Entries") + 
      theme_economist() 
bar_tag


##_________________EMP GROWTH RATE BY MARKET TAGS___________

sum_table_imp = sum_table
x = mean(sum_table_imp$emp_per_month, na.rm = T) 
sum_table_imp$emp_per_month[is.na(sum_table_imp$emp_per_month)==T] = x
tag_stats = sum_table_imp%>%
      filter(is.na(top_tag) == F)%>%
      group_by(top_tag)%>%
      summarise(avg_invests = mean(year_invests_inc),
                avg_growth_rate = mean(emp_per_month),
                count = n())


#______graph____

tag_stats = filter(tag_stats, count>5)
tag_stats$top_tag = factor(tag_stats$top_tag, levels = tag_stats$top_tag[order(tag_stats$avg_growth_rate)])

tag = ggplot(tag_stats, mapping = aes(top_tag, avg_growth_rate))
bar_tag = tag + geom_bar(stat = "identity", fill = "yellow") + 
      coord_flip() + 
      labs(title = "Average Employee Growth Rate by Top Market Tag (count>5)", x = "Tag", y = "Avg_Growth_Rate (Employees/month)") + 
      theme_economist() 
bar_tag



#_________________________MODELING__________________________________

model = lm(data = sum_table, formula = emp_per_month ~ top_tag)
model = aov(data = sum_table, formula = year_invests_inc ~ top_tag)
model
summary(model)
plot(model)

