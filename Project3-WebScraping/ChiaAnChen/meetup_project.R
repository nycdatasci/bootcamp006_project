##################################################
## NYC Data Science Academy                     ##
## Project 3: Web scraping                      ##
## Topic: meetup.com analysis and prediction    ##
## Author: Chia-An Chen                         ##
##################################################
# data scraping started at 08/10/2016 06:30pm till 10:30pm
# data was scraped from meetup.com using python code "project3_web.ipynb"
# data of meetup events within 10 miles of targeted cities are scraped
# cities scraped: New York, San Francisco, Chicago, DC, Palo Alto, Boston, LA, Mountain View, Seattle, Austin
#          abbv: ny, sf, chicago, dc, palo_alto, boston, la, mountain_view, seattle, austin

# load libraries
library(rjson)
library(dplyr)
library(chron)
library(stringr)
library(stringi)
library(tm)
library(wordcloud)
library(ggplot2)
library(tools)
library(randomForest)
library(stats)
library(glmnet)
library(car)
# library(VIM)
# library(mice)

######################
## Define Functions ## 
######################
# function to read data
read_raw_data = function(city){
  raw_df = read.csv(paste0("/Users/annecool37/Dropbox/DataScience/Project3/data_scraped/", city, "_meetup.csv"), header = TRUE)[-1]
  groups = fromJSON(file = paste0("/Users/annecool37/Dropbox/DataScience/Project3/data_scraped/", city, "group.json"))
  keywords = fromJSON(file = paste0("/Users/annecool37/Dropbox/DataScience/Project3/data_scraped/", city, "keyword.json"))
  return (list(raw_df, groups, keywords))
}

# function to reformat the dataframe
format_df = function(df, city_name) {
  result_df = select(df, -related_meetup, -location) %>%
    mutate("event_date" = as.Date(event_date, "%Y-%m-%d"),
           "event_day" = weekdays(as.Date(event_date, "%Y-%m-%d")),
           "event_start_time" = format(as.POSIXlt(event_start_time,format="%H:%M"), "%H:%M"),
           "founded_date" = as.Date(founded_date, "%Y-%m-%d"),
           "event_title" = tolower(as.character(event_title)), 
           "group_name" = tolower(as.character(group_name)), 
           "host_founded_diff" = round(as.numeric(difftime(event_date, founded_date))), 
           "upcoming_event_count" = as.numeric(gsub(",", "", upcoming_event_count)),
           "past_meetup_count" = as.numeric(gsub(",", "", past_meetup_count)), 
           "member_count" = as.numeric(gsub(",", "", member_count)),
           "review_count" = as.numeric(gsub(",", "", review_count)),
           "city" = city_name
    )
  # result_df$has_price = ifelse(is.na(df$price) == TRUE, "No", "Yes")
  # result_df$has_sponsor = ifelse(is.na(df$sponsor_count) == TRUE, "No", "Yes")
  return (result_df)
}

# function to section event_start_date into several groups
create_time_category = function(df){
  cut_time = c("03:00", "06:00", "09:00", "12:00", "15:00", "18:00", "21:00", "24:00")
  category = c("3am-6am", "6am-9am", "9am-12pm", "12pm-3pm", "3pm-6pm", "6pm-9pm", "9pm-12am")
  time_category = NULL
  for (i in seq(1,length(category))){
    time_category[which(df$event_start_time >= cut_time[i] & df$event_start_time < cut_time[i+1])] = category[i]
  }
  time_category[which(df$event_start_time < "03:00")] = "12am-3am"
  df$event_time_category = time_category
  return (df)
}

# declare words to be removed from frequency count
word_to_remove = c("nyc", "new jersey", "new york", "chicago", "seattle", "bay area", "austin", "boston", "silicon valley",
                   "washington", "los angeles", "mountain view", "san francisco", "central park",
                   "meetup", "meeting", "meet", "event", "group", "club", "events",
                   "north", "south", "east", "west", "area",
                   "city", "brooklyn", "jersey", "manhattan", "hoboken", "queens", "hudson",
                   "2016","august", "september", "october", 
                   "day", "monday", "tuesday", "wednesday",
                   "thursday", "friday", "saturday", "sunday")

# function to create corpus for counting word frequency
word_freq = function(value) {
  # remove quotes from the charaters
  text = noquote(value)
  # create a master string containg all the word
  text = paste(text, collapse=" ")
  text_source = VectorSource(text)
  corpus = Corpus(text_source)
  # remove punctuation
  corpus = tm_map(corpus, removePunctuation) 
  # convert all words to lower case
  corpus = tm_map(corpus, content_transformer(tolower)) 
  # remove basic stop words, check out stopwords("english") for the full list
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  # remove words indicating location, meetup, weekday, month, year
  corpus = tm_map(corpus, removeWords, word_to_remove) 
  dtm = as.matrix(DocumentTermMatrix(corpus)) 
  # get the word frequency
  frequency = sort(colSums(dtm), decreasing = TRUE)
  return (frequency)
  # return (corpus)
}

# # function to return a vector of "Yes" & "No" to check 
# # if an attribute contains top_n frequent word
# has_top = function(which_attribute, which_freq, top_n){
#   vec = ifelse(sapply(which_attribute, function(x) length(grep("TRUE", stri_detect_fixed(tolower(x), names(which_freq[1:top_n])))) != 0), "Yes", "No")
#   return (vec)
# }

# get the count of top n freqeunt word
get_top_count = function(freq_df, target_vec, top_n){
    count_vec = sapply(target_vec, function(x) sum(sapply(freq_df[,1][1:top_n], grepl, x), na.rm = TRUE))
    return (count_vec)
}

# function to get the accuracy of the random forest model 
get_accuracy = function(df){
    df = mutate(df, count_predicted = as.numeric(count_predicted))
    # cut point is detemined by viewing the distribution of the participant_count
    cut_num = c(1, 10, 20, 30, 50, 100)
    num_category = c("n <= 10", "10  < n <= 20", "20 < n <= 30", "30 < n <= 50", "50 < n <= 100")
    num_people_acutal = NULL
    num_people_predicted = NULL
    for (i in seq(1,length(num_category))){
        num_people_acutal[which(df$participant_count >= cut_num[i] & df$participant_count < cut_num[i+1])] = num_category[i]
        num_people_predicted[which(df$count_predicted >= cut_num[i] & df$count_predicted < cut_num[i+1])] = num_category[i]
    }
    num_people_acutal[which(df$participant_count > 100 )] = "n > 100"
    num_people_predicted[which(df$count_predicted > 100 )] = "n > 100"
    df$actual_count_group = num_people_acutal
    df$predicted_count_group = num_people_predicted
    df = mutate(df, "category_accuracy" = (actual_count_group == predicted_count_group))
    t = table(df$category_accuracy)
    accuracy = t["TRUE"]/(t["TRUE"] + t["FALSE"])
    return (accuracy)
}

# function to prettify the city name
prettify_city = function(df){
    pretify_name = c('NY', 'SF', 'Chicago', 'DC', 'Palo Alto', 'Boston','LA', 'Mountain View', 'Seattle', 'Austin')
    for (i in seq(1, 10)){
        df$city = gsub(city_name[i], pretify_name[i], df$city)
    }
    return (df)
}

#################################
## Data Reading and Processing ## 
#################################
city_name = c('ny', 'sf', 'chicago', 'dc', 'palo_alto', 'boston','la', 'mountain_view', 'seattle', 'austin')

# read data
for (name in city_name){
  assign(paste0('master', ''), read_raw_data(name))
  assign(paste0(name, '_raw'), master[[1]])
  assign(paste0(name, '_gr'), master[[2]])
  assign(paste0(name, '_key'), master[[3]])
}

# reformat the dataframe
raw_files = list(ny_raw, sf_raw, chicago_raw, dc_raw, palo_alto_raw, boston_raw, la_raw, mountain_view_raw, seattle_raw, austin_raw)
i = 1
for (name in city_name){
  assign(paste0(name, ''), format_df(raw_files[[i]], name))
  i = i+1
}

# combind all dataframes collected for 10 cities
master_df = do.call("rbind", list(ny, sf, chicago, dc, palo_alto, boston, la, mountain_view, seattle, austin))

# filter time range that ensure we are comparing amoung cities in the same time range
# filter data only from 08/11 to 10/07
check_date_df = group_by(master_df, city) %>% summarise("min" = min(event_date, na.rm = TRUE), "max" = max(event_date, na.rm = TRUE))
master_df = filter(master_df, event_date >= "2016-08-11" & event_date <= "2016-10-07")

# create new column: time category 
# e.g change event_start_time from 16:00 to afternoon
master_df = create_time_category(master_df)

# get the word frequency in event title and group name
title_freq = word_freq(master_df$event_title)
group_name_freq = word_freq(master_df$group_name)
# combind all group keywords and get the word freqency
keyword_list = c(ny_key, sf_key, chicago_key, dc_key, palo_alto_key, boston_key, la_key, mountain_view_key, seattle_key, austin_key)
keyword_freq = word_freq(keyword_list)

# turn frequency count into dataframes
title_freq_df = data.frame("word" = names(title_freq), "freq" = title_freq)
group_name_freq_df = data.frame("word" = names(group_name_freq), "freq" = group_name_freq)
keyword_freq_df = data.frame("word" = names(keyword_freq), "freq" = keyword_freq)

# top_n is deceided by the number of rows/100 to get 1% of the frequent word
top_title_n = round(nrow(title_freq_df)/100) # 106
top_gr_name_n = round(nrow(group_name_freq_df)/100) # 50
top_gr_keyword_n = round(nrow(keyword_freq_df)/100) # 77

# count word in title/group name/group keyword to see how many top 1% frequent words there are
master_df$top_title_count = get_top_count(title_freq_df, master_df$event_title, top_title_n) 
master_df$top_gr_name_count = get_top_count(group_name_freq_df, master_df$group_name, top_gr_name_n) 
master_df$top_gr_keyword_count = get_top_count(keyword_freq_df, master_df$group_keywords, top_gr_keyword_n) 

# # check the distribution of freqency and determine cutpoint
# plot(title_freq[1:100], main = "event title freq", ylab = "freq") # cutpoint: freq > 150
# plot(group_name_freq[1:100], main = "group name freq", ylab = "freq") # cutpoint: freq > 150
# plot(keyword_freq[1:100], main = "group keyword freq", ylab = "freq") # cutpoint: freq > 1000 
# # get an idea of how I should define n for top n used in the section below
# length(which(title_freq_df$freq > 150)) # ~25
# length(which(group_name_freq_df$freq > 150)) # ~20
# length(which(keyword_freq_df$freq > 1000)) # ~ 10
# # define n for top n and create new columns: has_top_group_name, has_top_event_title
# # "Yes" is the group name or event title has the top n frequent word, otherwise assign value "No" 
# master_df$has_top_event_title = has_top(master_df$event_title, title_freq, 25)
# master_df$has_top_group_name = has_top(master_df$group_name, group_name_freq, 20)
# master_df$has_top_group_keyword = has_top(master_df$group_keywords, keyword_freq, 15)
# # get a glance at how many "Yes" & "No" in the new columns created
# table(master_df$has_top_event_title)
# table(master_df$has_top_group_name)
# table(master_df$has_top_group_keyword)

####################
## Model Building ##
####################
# create dataframe for modeling by removing the following columns:
model_df = subset(master_df, select=-c(approval_needed, event_date, event_start_time, 
                                       event_title, founded_date, group_keywords, group_name, private_group))
# summary(model_df)
# # check missingness
# md.pattern(model_df)
# aggr(model_df)
# since missingness is caused by the fact that the event has no sponsor nor price requirement
# impute all missing value with 0
model_df[is.na(model_df)] = 0 
model_df = mutate(model_df, "comment_reply_count" = comment_count + reply_count) %>%
    rename("event_time" = event_time_category) %>%
    subset(select= -c(comment_count, reply_count))
factor_model_df = as.data.frame(unclass(model_df))
# names(factor_model_df)

## ~~~~~~~~~~~~~ ##
## Random Forest ##
## ~~~~~~~~~~~~~ ##
# # ref : http://trevorstephens.com/kaggle-titanic-tutorial/r-part-5-random-forests/
# # The accuracy one tests to see how worse the model performs without each variable, 
# # so a high decrease in accuracy would be expected for very predictive variables
set.seed(8)
# final model df selected: 
# convert all column with characters into factors
rf_model_df = factor_model_df
# names(reduced_model_df)
train_idx = sample(1:nrow(rf_model_df), 8*nrow(rf_model_df)/10)
tree_train = rf_model_df[train_idx,]
tree_test = rf_model_df[-train_idx,]
# check if the training and testing subset has similar participant_count distribution
# if not, the prediction will yield lower accuracy
par(mfrow = c(1,2))
plot(density(tree_train$participant_count), main = "Density of training set")
plot(density(tree_test$participant_count), main = "Density of testing set")
# decide the optimal mtry (which is 4)
tuneRF(tree_train[,-2], tree_train[,2], stepFactor=1.5)
fit_reduced = randomForest(as.factor(participant_count)~., data = tree_train, 
                      importance = TRUE, mtry = 4, ntree = 500) 

# check which variable is more important in our model
varImpPlot(fit_reduced, main = "Variable Importance Plot")
# predict the number of participant with our random forest model
prediction = predict(fit_reduced, tree_test)
result = data.frame("participant_count" = tree_test$participant_count, "count_predicted" = prediction)
# check accuracy of our model
# relative accuracy:
# if there are n people going, how accurate the model predict within a range
accuracy = get_accuracy(result)
accuracy

# absolute accuracy:
# if there are n people going, how accurate the model actaully predict exactly n people going
df = mutate(result, count_predicted = as.numeric(count_predicted)) %>%
    mutate("absolute_accuracy" = (participant_count == count_predicted))
t = table(df$absolute_accuracy)
t["TRUE"]/(t["TRUE"]+t["FALSE"])

## ~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Multiple Linear Regression ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
# try multiple linearregression, not really working....
# all assumptions are being violated since many of variables are correlated with each other
# model_lm = lm(participant_count ~., data = factor_model_df)
# summary(model_lm)
# par(mfrow = c(2,2))
# plot(model_lm)

## ~~~~~~~~~~~~~~~~ ##
## Lasso Regression ##
## ~~~~~~~~~~~~~~~~ ##
# subset training and testing data
x = model.matrix(participant_count ~ ., factor_model_df)[, -1] #Dropping the intercept column.
y = factor_model_df$participant_count
set.seed(15)
train = sample(1:nrow(x), 8*nrow(x)/10)
test = (-train)
y.test = y[test]
# define lambdas
grid = 10^seq(5, -2, length = 100)
lasso_model = glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso_model, xvar = "lambda", label = TRUE, main = "Lasso Regression")
# Running 5-fold cross validation.
set.seed(10)
cv_lasso = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 1, nfolds = 5)
plot(cv_lasso, main = "Lasso Regression\n")
bestlambda_lasso = cv_lasso$lambda.min
# bestlambda_lasso
# log(bestlambda_lasso)
lasso_all = glmnet(x, y, alpha = 1)
predict(lasso_all, type = "coefficients", s = bestlambda_lasso)

###############################
## Exploratory Data Analysis ##
###############################
# for the purpose of generating the wordcloud
# manually modify the keyword frequency to prevent repetitiveness 
keyword_freq["women"] = keyword_freq["women"] + keyword_freq["womens"]
keyword_freq["womens"] = 0
keyword_freq["professional"] = keyword_freq["professional"] + keyword_freq["professionals"]
keyword_freq["professionals"] = 0
# view all palettes by calling display.brewer.all()
pal = brewer.pal(9,"GnBu")[-(1:4)]
# pal = brewer.pal(9,"YlOrRd")[-(1:4)]
num_word = 100
# create word cloud
par(mfrow = c(1,1))
wordcloud(toTitleCase(names(title_freq)), title_freq, max.words = num_word, random.order = FALSE, colors=pal)
wordcloud(toTitleCase(names(group_name_freq)), group_name_freq, max.words = num_word, random.order = FALSE, colors=pal)
wordcloud(toTitleCase(names(keyword_freq)), keyword_freq, max.words = num_word, random.order = FALSE, colors=pal)
# # plot(group_name_freq)
# # bar plot if needed
# # !!!! cannot sort the bar based on frequency !!!!!
# g = ggplot(subset(title_freq_df, freq > 200), aes(word, freq))
# p = g + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust=1))   
# p  

# check how many data collected for each city
master_df = prettify_city(master_df)
count_df = group_by(master_df, city) %>% summarise("count" =n(), 
                                                   "participant_per_event" = sum(participant_count)/n()) 
g = ggplot(count_df, aes(x= reorder(city, -count), y = count)) 
b = geom_bar(stat="identity", fill="#de2d26", alpha = 0.8)
t = theme_bw() + theme(axis.text = element_text(size=20),
                       axis.title = element_text(size=20),
                       plot.title = element_text(size=25),
                       axis.text.x = element_text(angle = 45, hjust=1))
p = g + t + b +labs(title="Number of Event Collected in Ten Cities", 
                    x = " ", y = "Number of Event")
p

# # check which city has highest mean of participant/event 
g = ggplot(count_df, aes(x= reorder(city, -participant_per_event), y = participant_per_event)) 
p = g + t + b + labs(title="Average Number of Participants Per Event in Ten Cities", 
                     x = " ", y = "Average Number of Participants Per Event")
p

# check distribution of number of participant
par(mfrow = c(1,2))
plot(density(master_df$participant_count), main = "Density Plot", xlab = "Number of Participants")
plot(density(master_df$participant_count[master_df$participant_count <50]), main = "Density Plot", xlab = "Number of Participants")
# design number of participant cut point
# 1, 10, 20, 30, 50, 100, or more

## Heat Map ##
# best time to host an event
popular_df = group_by(master_df, event_time_category, event_day) %>% 
    summarise("event_count" = n(), "num_participant" = sum(participant_count), "people_per_event" = round(mean(participant_count, na.rm = TRUE)))

day_levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
prettify_day = c('Mon', 'Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun')
for (i in seq(1, 10)){
    popular_df$event_day = gsub(day_levels[i], prettify_day[i], popular_df$event_day)
    }
popular_df$event_day = factor(popular_df$event_day, levels = c('Mon', 'Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun'), ordered = TRUE)
popular_df$event_time_category = factor(popular_df$event_time_category, levels = c("3am-6am","6am-9am", "9am-12pm", "12pm-3pm", "3pm-6pm", "6pm-9pm", "9pm-12am", "12am-3am"), ordered = TRUE)
# filter event count >3 to avoid bias (say if the category only has one event, it has too much leverage)
popular_df = filter(popular_df, event_count > 3)
g = ggplot(popular_df,aes(event_day, event_time_category), xlab = NULL, ylab = NULL)
t = theme_bw() + theme(axis.title = element_blank(), 
                       axis.text = element_text(size = 20),
                       legend.text = element_text(size = 20),
                       legend.title = element_text(size = 20))
# create heatmaps based ont event count, number of participants, and participants per event
p_event = g + t + geom_tile(aes(fill = event_count),colour = "white") + scale_fill_gradient(low = "white", high = "#de2d26", name = "Number of\nEvents")  
p_participant = g + t + geom_tile(aes(fill = num_participant),colour = "white") + scale_fill_gradient(low = "white", high = "#de2d26", name = "Number of\nParticipants")  
p_ppl_per_event = g + t + geom_tile(aes(fill = people_per_event),colour = "white") + scale_fill_gradient(low = "white", high = "#de2d26", name = "Number of\nParticipants\nPer Event")  

p_event
p_participant
p_ppl_per_event

# # group by city and find frequent event time and event day
trend_df = group_by(master_df, city, event_time_category, event_day) %>% 
    summarise("event_count" = n(), "num_participant" = sum(participant_count), 
              "people_per_event" = mean(participant_count, na.rm = TRUE)) %>%
    filter(event_count > 3)
test_df = mutate(trend_df, "time_n_day" = paste0(event_day,", " ,event_time_category)) %>% 
    filter(num_participant != 114) %>% select(1,6,7)   # this is an...extreme case where 1 event shift the result
test_df = test_df[-1]
df_top = group_by(test_df, city) %>% mutate(time_n_day = time_n_day, "top_count" = max(people_per_event)) %>%
    filter(top_count == people_per_event)
# View(df_top)
# write.csv(df_top, "top_ppl_per_event_count.csv")

# # boxplot num vs. city
# numeric_df = subset(master_df, select = c(city, member_count, host_founded_diff, past_meetup_count, upcoming_event_count, participant_count))
# ggplot(master_df) + geom_boxplot(aes(x = city, y = participant_count)) # + facet_grid( ~ city)

# # Thursday 6-9pm is the most popular time to host an event across all 10 cities!
# trend_df = group_by(master_df, city, event_time_category, event_day) %>% 
#     summarise("event_count" = n()) 
# test_df = mutate(trend_df, "time_n_day" = paste0(event_day,", " ,event_time_category)) %>% select(1,4,5) 
# test_df = test_df[-1]
# df_top = group_by(test_df, city) %>% mutate(time_n_day = time_n_day, "top_count" = max(event_count)) %>%
#     filter(top_count == event_count)
# write.csv(df_top, "top_event_count.csv")

# trend_df = group_by(master_df, city, event_day) %>% 
#     summarise("event_count" = n(), "num_participant" = sum(participant_count), "people_per_event" = round(mean(participant_count, na.rm = TRUE)))
# df_top_day = group_by(trend_df, city) %>% mutate(event_day = event_day, "max_people_per_event" = max(people_per_event)) %>%
#     filter(max_people_per_event == people_per_event)
# df_top_day = df_top_day[,c(1,2,6)]
# write.csv(df_top_day, "top_day.csv")
# View(df_top_day)

# trend_df = group_by(master_df, city, event_time_category) %>% 
#     summarise("event_count" = n(), "num_participant" = sum(participant_count), "people_per_event" = round(mean(participant_count, na.rm = TRUE)))
# df_top_time = group_by(trend_df, city) %>% mutate(event_time_category = event_time_category, "max_people_per_event" = max(people_per_event)) %>%
#     filter(max_people_per_event == people_per_event)
# df_top_time = df_top_time[,c(1,2,6)]
# write.csv(df_top_time, "top_time.csv")
# View(df_top_time)

########################
## Miscellaneous Note ##
########################
# subset private group out if needed
# table(master_df$private_group) 
# before specifying time range, I got
#    No   Yes 
# 12891  4348
# from running table(master_df$private_group) 
# but once I only focus on data with event time from 08/10/2016 to 10/07/2016
# table(master_df$private_group)
# table(master_df$approval_needed)
# No provate groups and no approval needed! (maybe it's time-sensitive?)
# private_df = master_df[which(master_df$private_group == "Yes"), ]


# build a webpage allowing user to select location
# popup variables needed for that model
# key in value --> show number of participant prediction

# create neural network plot to visualize if a group is associate with one another
# related_gr_list = c(ny_gr, sf_gr, chicago_gr, dc_gr, palo_alto_gr, boston_gr, la_gr, mountain_view_gr, seattle_gr, austin_gr)

# ny = format_df(ny_raw)
# ny = mutate(ny, "state" = str_sub(as.character(location),-2,-1))
# # use unique(nyc$state) to validate the event 
# # and I manually figured out that some events should not be in the dataset
# # like events in CT and HI should not show up in the NY data
# vec = c("CT", "HI")
# weird_case_idx = which(ny$state %in% vec == TRUE)
# # remove event that are not within 10 miles of NY
# ny = ny[-weird_case_idx, ]
# # for easier comparison with other cities
# # even NJ is changed into NY (138 observations)
# ny = mutate(ny, "state" = "NY") 
# # Weird Events Record in the search result within 10 miles in New York, NY
# # Farm to Table Potluck @ Brooklyn, CT
# # http://www.meetup.com/Events-Gone-Wild/events/230102046/
# # Back-Yard Summer Olympics / Potluck @ Brooklyn , CT
# # http://www.meetup.com/Events-Gone-Wild/events/230096553/
# # Tastes of Kauai 2016 @ Kauai, HI
# # http://www.meetup.com/Kauai-Adventure-Meetup/events/232556020/
