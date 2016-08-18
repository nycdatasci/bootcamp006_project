setwd("/Users/williambartlett/PycharmProjects/Web_scraping_project/Excel_files")
#setwd("/Users/williambartlett/PycharmProjects/Web_scraping_project/Rfiles")
library(readxl)
library(dplyr)
library(ggplot2)
library(stats)
library(stringr)
library(tidyr)
library(wordcloud)
library(ggthemes)
#load("info_df.RData")

info_df = read_excel('info_df.xlsx')
colnames(info_df) = c("Company", "Blurb", "Location", "Market_Tags", "Employee_num_Range")

class(info_df$Market_Tags)
info_df_rev = info_df
info_df_rev$Market_Tags = gsub("[", "", info_df_rev$Market_Tags, fixed = T)
info_df_rev$Market_Tags = gsub("]", "", info_df_rev$Market_Tags, fixed = T)
info_df_rev$Market_Tags = gsub("u'", "", info_df_rev$Market_Tags, fixed = T)
info_df_rev$Market_Tags = gsub("'", "", info_df_rev$Market_Tags, fixed = T)
info_df = info_df_rev

info_df$num_locations = str_count(info_df$Location, pattern = "·")
num_locations = str_count(info_df$Location, pattern = "·") + 1
info_df$num_locations = num_locations

info_df$Blurb = tolower(info_df$Blurb)

#indices
indices = c(
grep("ceo", info_df$Blurb),
grep("coo", info_df$Blurb),
grep("founder", info_df$Blurb),
grep("fellow", info_df$Blurb),
grep("consultant", info_df$Blurb),
grep("university", info_df$Blurb),
grep("investor", info_df$Blurb),
grep("partner", info_df$Blurb),
grep("founding", info_df$Blurb),
grep("found", info_df$Blurb)
)

info_df$Blurb[indices] = NA
#_________________________________________

info_df$market_tags_save = info_df$Market_Tags

info_df = separate(data = info_df, 
                    col = market_tags_save, 
                    into = paste("Market Tag", c(1:7)), 
                    sep = ",")

max(str_count(info_df$market_tags_save, ','))


paste("Market Tag", c(1:7))

info_df_marktags = info_df
#______________________________________________________________

info_df_marktags = info_df_marktags[,-c(11,12,13)]

sum(complete.cases(info_df_marktags$`Market Tag 4`))

markettags_1col = info_df_marktags[,c(10,9,8,7)]
markettags_1col = stack(markettags_1col)
markettags_1col$values = gsub("^\\s+|\\s+$", "", markettags_1col$values)
markettags_1col = data.frame(markettags_1col$values)
colnames(markettags_1col) = "tag"

markettags_1col = filter(markettags_1col, is.na(markettags_1col$tag) == F)
table = count(markettags_1col, tag)
table = arrange(table, desc(n))
top_table = table[c(4:34),]
top_table$tag = factor(top_table$tag, levels = top_table$tag[order(top_table$n)])
#__________________________________________________________________

g = ggplot(top_table, mapping = aes(x = tag, y = n))
bar = g + geom_bar(stat = "identity", fill = "yellow") + 
      coord_flip() + 
      labs(title = "Frequency of Market Tags in 400 Health Care Start-ups on Angelist.com", x = "Market Tag", y = "Count") + 
      theme_economist() 
bar
#____________________________________________________________________

by_tag_num = info_df_marktags[,c(10,9,8,7)]
by_tag_num = stack(by_tag_num)
by_tag_num$values = gsub("^\\s+|\\s+$", "", by_tag_num$values)
colnames(by_tag_num) = c("Tag","Tag_Num")
by_tag_num = filter(by_tag_num, is.na(by_tag_num$Tag) == F)
one = filter(by_tag_num, Tag_Num == "Market Tag 1")
two = filter(by_tag_num, Tag_Num == "Market Tag 2")
three = filter(by_tag_num, Tag_Num == "Market Tag 3")
four = filter(by_tag_num, Tag_Num == "Market Tag 4")
View(four)

create_bar = function(x, number_th){
      tabled = count(x, Tag)
      tabled = arrange(tabled, desc(n))
      top_tabled = tabled[c(4:34),]
      top_tabled$Tag = factor(top_tabled$Tag, levels = top_tabled$Tag[order(top_tabled$n)])
      
      g = ggplot(top_tabled, mapping = aes(x = Tag, y = n))
      bar = g + geom_bar(stat = "identity", fill = "yellow") + 
            coord_flip() + 
            labs(title = paste0("Frequency as ", number_th, " MarketTag"), x = "Market Tag", y = "Count") + 
            theme_economist() +
            scale_y_continuous(limit = c(0, 120))
      bar
}
create_bar(one, "First")
create_bar(two, "Second")
create_bar(three, "Third")
create_bar(four, "Fourth")


wordcloud(words = table$tag, freq = table$n, min.freq = 3)
