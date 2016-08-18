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

#remove last 3 columns
info_df_marktags = info_df_marktags[,-c(11,12,13)]

#create one column df list of market tags
markettags_1col = info_df_marktags[,c(10,9,8,7)]
markettags_1col = stack(markettags_1col)
markettags_1col$values = gsub("^\\s+|\\s+$", "", markettags_1col$values)
markettags_1col = data.frame(markettags_1col$values)
colnames(markettags_1col) = "tag"
markettags_1col = filter(markettags_1col, is.na(markettags_1col$tag) == F)

# create freq table and top 30 freq table/ reorder factors for graph
table = count(markettags_1col, tag)
table = arrange(table, desc(n))
top_table = table[c(1:30),]
top_table$tag = factor(top_table$tag, levels = top_table$tag[order(top_table$n)])

#create first graph
g = ggplot(top_table, mapping = aes(x = tag, y = n))
bar = g + geom_bar(stat = "identity", fill = "yellow") + 
      coord_flip() + 
      labs(title = "Frequency of Market Tags in 400 Health Care Start-ups on Angelist.com", x = "Market Tag", y = "Count") + 
      theme_economist() 
bar

#create tag list with tag position
by_tag_num = info_df_marktags[,c(10,9,8,7)]
by_tag_num = stack(by_tag_num)
by_tag_num$values = gsub("^\\s+|\\s+$", "", by_tag_num$values)
colnames(by_tag_num) = c("Tag","Tag_Num")

#break down by position
by_tag_num = filter(by_tag_num, is.na(by_tag_num$Tag) == F)
one = filter(by_tag_num, Tag_Num == "Market Tag 1")
two = filter(by_tag_num, Tag_Num == "Market Tag 2")
three = filter(by_tag_num, Tag_Num == "Market Tag 3")
four = filter(by_tag_num, Tag_Num == "Market Tag 4")

#function to create individual graphs
create_bar = function(x, number_th){
      tabled = count(x, Tag)
      tabled = arrange(tabled, desc(n))
      top_tabled = tabled[c(1:30),]
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

#create wordcloud
cloud = count(markettags_1col, tag)

wcloud = wordcloud(words = cloud$tag, freq = cloud$n, min.freq = 3)

