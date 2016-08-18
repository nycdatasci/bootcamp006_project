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

blurb = select(info_df_marktags, Blurb)
blurb_stacked = stack(blurb)
blurb_stacked = blurb_stacked[complete.cases(blurb_stacked),]
colnames(blurb_stacked) = c('blurb', 'index')

blurbs = paste(blurb_stacked$blurb[1:366], collapse = ' ')
blurbs = str_replace_all(blurbs, "[[:punct:]]", "")
blurbs = gsub('[0-9]+', '', blurbs)
blurbs_v = strsplit(blurbs, " ")
blurbs_v = unlist(blurbs_v)
blurbs_v_df = data.frame(blurbs_v)
blurbs_v_df$blurbs_v = gsub("^\\s+|\\s+$", "", blurbs_v_df$blurbs_v)
blurbs_v_count = count(blurbs_v_df, blurbs_v)
View(blurbs_v_count)
blurbs_v_count$length = str_length(blurbs_v_count$blurbs_v)
blurbs_v_count = blurbs_v_count[blurbs_v_count$length>5,]
blurbs_v_count = arrange(blurbs_v_count, desc(n))
top_blurb_words = blurbs_v_count[c(5:40),]
top_blurb_words$blurbs_v = factor(top_blurb_words$blurbs_v, 
                                        levels = top_blurb_words$blurbs_v[order(top_blurb_words$n)])
      

g = ggplot(top_blurb_words, mapping = aes(x = blurbs_v, y = n))
bar = g + geom_bar(stat = "identity", fill = "yellow") + 
      coord_flip() + 
      labs(title = "Most Frequent Blurb Words (length>5)", x = "Blurb Words", y = "Count") + 
      theme_economist()
bar

#check individual words
filter(blurbs_v_count, blurbs_v_count$blurbs_v=="genetic")

wordcloud(blurbs_v_count$blurbs_v, freq = blurbs_v_count$n)
