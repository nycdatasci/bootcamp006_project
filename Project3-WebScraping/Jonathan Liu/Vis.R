library(ggplot2)
library(data.table)
library(dplyr)
library(dtplyr)
library(stringr)
library(wordcloud)
library(tm)
library(RColorBrewer)

jobs <- fread('unique_jobs.csv')

#### Result Count by Search Terms ####
term_count_df <- data.frame(table(jobs$search_term)) %>% 
  rename(Term = Var1) %>%
  mutate(Term = factor(Term, levels = Term[order(Freq)]))

ggplot(data = term_count_df, border = F) + 
  geom_bar(aes(x = Term, y = Freq, fill = Term),
           stat = 'identity') +
  geom_text(aes(x = Term, y = Freq - 150, label = Freq)) +
  coord_flip() + theme(legend.position = 'none',
                       axis.title.x = element_text(colour = "white"),
                       axis.text=element_text(size = 18, colour = 'white'),
                       plot.background = element_rect(fill = '#3F3F3F')) + 
  labs(x = NULL, y = '# of Results')

#### Wordcloud for Job Functions ####
fun <- fread('fun.csv') %>%
  arrange(desc(Count))
fun$Function[1] <- 'IT'

pal = colorRampPalette(brewer.pal(3, "Dark2"))(35)

jpeg('rplot.jpg', width = 800, height = 600)
wordcloud( words = paste(fun$Function, '-', fun$Count),
           freq = fun$Count,
           scale = c(8, 1),
           rot.per = .1,
           random.order = F,
           random.color = T,
           colors = pal
           )
dev.off()

#### Industry for Each Term ####
industry <- fread('IndustryTop3.csv')

ggplot(data = industry) +
  geom_bar(aes(x = Industry, y = get('Ratio(%)') / 100,
               fill = Industry), 
           stat = 'identity') +
  geom_text(aes(x = Industry, y = (get('Ratio(%)') - 5) / 100,
                label = round( get('Ratio(%)'), 1 ),
                size = 10)) +
  facet_wrap( ~ Term) + 
  theme(legend.position = 'none',
        axis.title.x = element_text(colour = "white"),
        axis.text=element_text(size = 12, colour = 'white'),
        plot.background = element_rect(fill = '#3F3F3F')) + 
  labs(x = NULL, y = 'Ratio(%) of Result') + coord_flip()

#### Posting Date ####
postdate <- fread('PostDateDensity.csv')

weekday_label = weekdays(postdate$V1 + 3)

# Light to deep, remove lightest
pal = brewer.pal(9, "Blues")[3:9]
# asc(Num_list) order, shift sunday to beginning
pal = pal[rank(postdate$Num_List)][c(7, 1:6)]

ggplot( data = postdate ) +
  geom_bar( aes( x = weekday_label, y = Num_List), 
            fill = pal, stat = 'identity') +
  geom_text( aes( x = weekday_label, y = Num_List + 20, 
                  label = Num_List ), size = 8 ) +
  theme(axis.title.y = element_text(size = 20, colour = "white"),
        axis.text = element_text(size = 20, colour = 'white'),
        plot.background = element_rect(fill = '#3F3F3F')) +
  labs(x = NULL, y = 'Number of Job Listing')
