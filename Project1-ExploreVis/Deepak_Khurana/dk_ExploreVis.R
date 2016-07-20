#Loading data from YELP Dataset Challenge (Round 7)

library(jsonlite)                  # library to read data provided in .json file format

setwd('~/Downloads/yelp_dataset_challenge_academic_dataset/')

#checkin  = stream_in(file("yelp_academic_dataset_checkin.json"))   # read checkin data to checkin df
business = stream_in(file("yelp_academic_dataset_business.json"))   # read business data to business df
#tip      = stream_in(file("yelp_academic_dataset_tip.json"))       # read tip data to tip df
user     = stream_in(file("yelp_academic_dataset_user.json"))      # read user data to user df
#review   = stream_in(file("yelp_academic_dataset_review.json"))    # read review data to review df


#checkin_flat  = flatten(checkin)
business_flat = flatten(business)
#tip_flat      = flatten(tip)
user_flat     = flatten(user)
#review_flat   = flatten(review)



library(tibble)

#checkin_tbl   = as_data_frame(checkin_flat)
business_tbl  = as_data_frame(business_flat)
#tip_tbl       = as_data_frame(tip_flat)
user_tbl      = as_data_frame(user_flat)
#review_tbl    = as_data_frame(review_flat)

library(dplyr)
library(tidyr)
library(ggplot2)

business_tbl %>%                         #Total number of reviews
  summarise(sum(review_count))


data = business_tbl %>%                  #Business distribution as a function of ratings
  group_by(stars)%>% 
  count()

ggplot(data,aes(x=stars,y=n)) +               
  geom_bar(stat="identity",aes(fill=stars)) +
  xlab('stars') + ylab('Number of Businesses') +
  ggtitle("Business distribution as a function of ratings ") +
  theme_bw()

ggsave("Business_ratings.png")



data = business_tbl %>%                         #Average number of reviews vs ratings 
  group_by(stars)%>% 
  summarise(avg_rc=trunc(mean(review_count)))


ggplot(data,aes(x=stars,y=avg_rc)) +              
  geom_bar(stat="identity",aes(fill=stars)) +
  xlab('stars') + ylab('Number of reviews') +
  ggtitle("Average number of reviews vs ratings ") +
  theme_bw()

ggsave("avg_rv_ratings.png")


data = business_tbl %>%                         #Top 10 States in terms of number of reviews
  group_by(state)%>% 
  summarise(trc= sum(review_count))%>% 
  arrange(desc(trc))%>%
  head(10)

ggplot(data,aes(x=reorder(state, trc),y=trc)) +              
  geom_bar(stat="identity",aes(fill=state)) +
  xlab('')+ ylab('Number of reviews') +
  ggtitle("Top 10 States in terms of number of reviews") +
  coord_flip() +
  theme_bw()

ggsave("top_10_states.png")



data = business_tbl %>%                         #Top 10 cities in terms of number of reviews
  group_by(city)%>% 
  summarise(trc= sum(review_count))%>% 
  arrange(desc(trc))%>%
  head(10)

ggplot(data,aes(x=reorder(city, trc),y=trc)) +              
  geom_bar(stat="identity",aes(fill=city)) +
  xlab('')+ ylab('Number of reviews') +
  ggtitle("Top 10 cities in terms of number of reviews") +
  coord_flip() +
  theme_bw()

ggsave("top_10_cities.png")


ggsave("top_10_cities.png")

business_tbl %>%                         #Top 10 businesses in terms of review counts
  select(name,city,state,review_count) %>%
  arrange(desc(review_count)) %>%
  head(10)

#Restaurant Business

library(stringr)

business_tbl %>%                                     # Number of restaurants 
  filter(str_detect(categories, "Restaurant")) %>%
  count()


business_tbl %>%                                     # Top 10 restaurants in terms of most reviewed
  filter(str_detect(categories, "Restaurant")) %>%
  select(name,state,city,stars,review_count)%>%
  arrange(desc(review_count)) %>%
  head(10)




business_tbl %>%                                     #Unnest categories among restaurant 
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  select(name, categories)



business_tbl %>%                                     #Top 10 categories in  restaurant 
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  select(name, categories) %>%
  count(categories) %>%
  arrange(desc(n)) 

business_tbl %>%                                     #Top restaurant category for each state with atleast 10 count
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants") %>%
  count(state, categories) %>% 
  filter(n > 10) %>%
  top_n(1, n)



business_tbl %>%                               # Business with most number of categories 
  mutate(cat_count= sapply(business_tbl$categories,length)) %>% 
  select(name,cat_count)%>% 
  arrange(desc(cat_count))


restaurants_tbl = business_tbl %>%                 # Restaurants table 
  filter(str_detect(categories, "Restaurant"))

restaurants_tbl = business_tbl %>%                             #Unnest categories 
    unnest(categories)

restaurants_tbl %>%                                # Number of business vs categories 
  mutate(cat_count= sapply(restaurants_tbl$categories,length)) %>% 
  group_by(cat_count)%>% 
  count()

ggplot(data,aes(x=cat_count,y=n)) +              
  geom_bar(stat="identity",aes(fill=cat_count)) +
  xlab('Number of categories')+ ylab('Number of Business') +
  ggtitle("Number of business vs categories") +
  coord_flip() +
  theme_bw()

ggsave(" business_vs_categories.png")



b_tbl = business_tbl %>%                             #Unnest categories 
  unnest(categories)


b_tbl %>%                                    #Number of reviews by business categories
  group_by(categories)%>%
  summarise("total_rc"= sum(review_count))%>%
  arrange(desc(total_rc))



data = business_tbl %>%                      #Top 10 States in terms of number of business
  group_by(state)%>% 
  count()%>%
  arrange(desc(n))%>%
  head(10)

ggplot(data,aes(x=reorder(state, n),y=n)) +              
  geom_bar(stat="identity",aes(fill=state)) +
  xlab('')+ ylab('Number of business') +
  ggtitle("Top 10 States in terms of number of business") +
  coord_flip() +
  theme_bw()

ggsave("top_10_states_business.png")



data = business_tbl %>%                      #Top 10 States in terms of number of business
  group_by(city)%>% 
  count()%>%
  arrange(desc(n))%>%
  head(10)

ggplot(data,aes(x=reorder(city, n),y=n)) +              
  geom_bar(stat="identity",aes(fill=city)) +
  xlab('')+ ylab('Number of business') +
  ggtitle("Top 10 Cities in terms of number of business") +
  coord_flip() +
  theme_bw()

ggsave("top_10_cities_business.png")



data = business_tbl %>% 
  unnest(categories) %>% 
  arrange(stars) 

ggplot(data,aes(x=state,y=review_count))+
  geom_bar(stat= "identity",aes(fill=stars),position="fill") +
  coord_flip() +
  theme_bw()

business_tbl %>%                                     #Top restaurant category for each state with atleast 10 count
  filter(str_detect(categories, "Restaurant")) %>%
  unnest(categories) %>%
  filter(categories != "Restaurants")
  count()


