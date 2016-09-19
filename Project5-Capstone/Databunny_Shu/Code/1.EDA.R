########## EDA ##############
library(dplyr)
library(ggplot2)

consumption <- read.table('./data/consumption_recode.txt', sep = ',', header = T)
user <- read.table('./data/user_info.txt', sep = ',', header = T)
tag <- read.table('./data/rong_tag.txt', sep = ',', header = T)
relation1 <- read.table('./data/relation1.txt', sep = ',', header = T)
relation2 <- read.table('./data/relation2.txt', sep = ',', header = T)
test_with_random_lable <- read.table('./data/test_with_random_lable.txt', sep = ',', header = T)


########## Consumption Recode ##########
########################################
#length(unique(consumption$bill_id))
#[1] 677540 = number of columns 677540, bill_id is unique
#length(unique(consumption$user_id))
#[1] 23066 < 677540, one user has multiple bills






















############## Users Info ##############
########################################

###### Check the number of users #######
# length(unique(user$user_id))
# [1] 38261 < number of columns 253006, we need to remove repeated rows
'''
> str(user)
'data.frame':	253006 obs. of  22 variables:
  $ user_id        : Factor w/ 38261 levels "0000e1553a7fa79ba787b84ad21a3005",..: 35354 35354 35354 35354 35354 35354 35354 35354 35354 35354 ...
$ age            : Factor w/ 45 levels "17","18","19",..: 25 25 25 25 25 25 45 25 25 25 ...
$ sex            : int  1 1 1 1 1 1 0 1 1 1 ...
$ expect_quota   : int  50000 50000 50000 50000 50000 50000 50000 50000 50000 50000 ...
$ max_month_repay: logi  NA NA NA NA NA NA ...
$ occupation     : int  2 2 2 2 2 2 2 2 2 2 ...
$ education      : int  2 2 2 2 2 2 2 2 2 2 ...
$ marital_status : int  3 3 3 3 3 3 3 3 3 3 ...
$ live_info      : int  1 1 1 1 1 1 1 1 1 1 ...
$ local_hk       : int  1 1 1 1 1 1 1 1 1 1 ...
$ money_function : int  3 3 3 3 3 0 0 0 0 0 ...
$ company_type   : int  1 1 1 1 1 1 1 1 1 1 ...
$ salary         : int  3000 3000 3000 3000 3000 3000 3000 3000 3000 3000 ...
$ school_type    : int  0 0 0 0 0 0 0 0 0 0 ...
$ flow           : int  0 0 0 0 0 0 0 0 0 0 ...
$ gross_profit   : num  0 0 0 0 0 0 0 0 0 0 ...
$ business_type  : int  0 0 0 0 0 0 0 0 0 0 ...
$ business_year  : int  0 0 0 0 0 0 0 0 0 0 ...
$ personnel_num  : int  0 0 0 0 0 0 0 0 0 0 ...
$ pay_type       : int  0 0 0 0 0 0 0 0 0 0 ...
$ product_id     : int  1 1 1 1 1 1 1 1 1 1 ...
$ tm_encode      : int  21738909 21738715 21738635 21738715 21738569 624347 624124 624272 624293 624322 ...
'''

user_notime <- user[, -c(22)] # remove the tm_encode column
user_time <- user$tm_encode # record the tm_encode column
user_time <- as.data.frame(user_time)
#user_time_ord <- user_time %>% 
 # arrange(user_time)


user_notime_unique <- user_notime[!duplicated(user_notime), ] # remove rows with exactly same user information


# scatter plot1: check the distribution of tm_encode
ggplot(user_time, aes(x = c(1:253006), y = user_time)) + 
  geom_point(stat = 'identity') + 
  labs(x = 'Observations', y = 'Time of encoding', title = 'Distribution of encoding time') +
  theme_bw()
## 4 clusters for observations
# 60927, 123098, ... , ...

# scatter plot1.1: check the distribution of ordered tm_encode
user_ord <- arrange(user, desc(tm_encode))
ggplot(user_ord, aes(x = c(1:253006), y = tm_encode)) + 
  geom_point(stat = 'identity') + 
  labs(x = 'Observations', y = 'Time of encoding', title = 'Distribution of ordered encoding time') +
  theme_bw()
## 4 clusters for observations
# 60927, 123098, ... , ...



# scatter plot3.1: check the distribution of tm_encode in user_clean
ggplot(user_clean, aes(x = c(1:38260), y = tm_encode)) + 
  geom_point(stat = 'identity') + 
  labs(x = 'Observations', y = 'Time of encoding', title = 'Distribution of unique encoding time') +
  theme_bw()

# scatter plot3.2: check the distribution of tm_encode in user_clean in order of tm_encode
user_clean_ord <- arrange(user_clean, desc(tm_encode))
ggplot(user_clean_ord, aes(x = c(1:38260), y = tm_encode)) + 
  geom_point(stat = 'identity') + 
  labs(x = 'Observations', y = 'Time of encoding', title = 'Distribution of ordered unique encoding time') +
  theme_bw()

# density plot4.1: check the distribution of tm_encoding with density plot
ggplot(user, aes(tm_encode)) + 
  geom_density() + 
  labs(x = 'Time of encoding', y = 'Density', title = 'Distribution of encoding time with density plot') +
  theme_bw()

# density plot4.1: check the distribution of tm_encoding with density plot
ggplot(user_clean, aes(tm_encode)) + 
  geom_density() + 
  labs(x = 'Time of encoding', y = 'Density', title = 'Distribution of unique encoding time with density plot') +
  theme_bw()




# scatter plot: check the trend of the number of rows for every single user_id
## with duplicates 
nrow_user <- user %>% 
  group_by(user_id) %>% 
  summarise(nrows = n()) %>% 
  group_by(nrows) %>% 
  summarise(nusers = n())

ggplot(nrow_user, aes(nrows, nusers)) + 
  geom_point() + 
  labs(x = 'number of information rows', y = 'number of users', title = 'Users inforamtion 1') + 
  theme_bw()
# number of information rows: 6 (most) 

# Keep this dataframe with information rows and user_id
nrow_user_df <- user %>% 
  group_by(user_id) %>% 
  summarise(nrows = n())

## without duplicates
nrow_user_unique <- user_notime_unique %>% 
  group_by(user_id) %>% 
  summarise(nrows = n()) %>% 
  group_by(nrows) %>% 
  summarise(nusers = n())

ggplot(nrow_user_unique, aes(nrows, nusers)) + 
  geom_point() + 
  labs(x = 'number of information rows', y = 'number of users', title = 'Users inforamtion 2_unique') + 
  theme_bw()
# number of unique information rows: 2 (most) 

# Keep this dataframe with unique information rows and user_id
nrow_user_unique_df <- user_notime_unique %>% 
  group_by(user_id) %>% 
  summarise(nrows_unique = n()) 

#################### script zone #############################
user_notime_unique_none <- user_notime_unique[user_notime_unique$age %in% 'NONE' | user_notime_unique$age == 0, ] # check the type of missingness in 'age' and 'sex'
# 3 trypes:
# NONE 0: 31284
# NONE 1: 31
# NONE 2: 12
user_notime_unique_nonone <- user_notime_unique[!(user_notime_unique$age %in% 'NONE' | user_notime_unique$age == 0), ] # remove rows with missingness in 'age' or 'sex'
# 38260 users left, only one missed, good!

######## We get the information of users with largest tm_encode (lastest data)
user_clean0 <- arrange(user, desc(tm_encode)) # reorder by time 
user_clean <- user_clean0[!duplicated(user_clean0[, 1]), ]
user_clean <- merge(user_clean, nrow_user_unique_df, by = 'user_id') # add one column about unique rows
user_clean <- merge(user_clean, nrow_user_df, by = 'user_id') # add one column about rows
user_clean <- user_clean[-13368, ] # remove one row with abnormal value in 'live_info' and 'company_type'
rownames(user_clean) <- c(1:38260)
write.csv(user_clean, file = 'user_clean.csv')
# only 4 observations with missingness in 'age' and 'sex', this mehtod is acceptable,
# so we needn't deal with missingness in 'age' and 'sex' in advance.


####### Check missingness ###########
miss_NA <- user_clean[, -c(5, 10, 11, 14:20)] # remove variables changed with product type
miss_NA[, -c(4, 10)][miss_NA[, -c(4, 10)] == 0] <- NA  # substitute 0 with NA except 'expect_quota' and 'salary'

library(VIM)
aggr(miss_NA, prop = T, number = F, label = T, gap = T, only.miss = T)
summary(aggr(miss_NA, prop = T, number = T))

'''
 Missings per variable: 
Variable Count
user_id     0
age     0
sex     4
expect_quota     0
occupation     2
education     2
marital_status     6
live_info  9175
company_type 17039
salary     1
product_id     0
tm_encode     0

Missings in combinations of variables: 
Combinations Count      Percent
0:0:0:0:0:0:0:0:0:0:0:0 21221 55.463788192
0:0:0:0:0:0:0:0:1:0:0:0  7862 20.548339040
0:0:0:0:0:0:0:0:1:1:0:0     1  0.002613627
0:0:0:0:0:0:0:1:1:0:0:0  9167 23.959122867
0:0:0:0:0:0:1:0:1:0:0:0     1  0.002613627
0:0:0:0:0:0:1:1:1:0:0:0     5  0.013068137
0:0:1:0:0:0:0:0:0:0:0:0     1  0.002613627
0:0:1:0:0:0:0:1:1:0:0:0     1  0.002613627
0:0:1:0:1:1:0:1:1:0:0:0     2  0.005227255
'''

row.names(miss_NA)[rowSums(is.na(miss_NA)) > 0]
#[1] "13368"  : 2 missingnesses are in this row
sum(is.na(miss_NA))
#[1] 2 : 2 missingness in total

# plot a correlation plot
miss <- user_clean[, -c(5, 10, 11, 14:20)]
miss$age <- as.numeric(miss$age)
library(corrplot)
M <- cor(miss[-13368, -c(1)]) # remove 13368 row with missingness
corrplot(M, method = 'number')


############## rong tag ################
########################################
# number of rows = 687374
# length(unique(tag$user_id))
#[1] 16890
# length(unique(tag$rong_tag))
#[1] 37359


# density plot1: check the distribution of rong_tag with density plot
ggplot(tag, aes(rong_tag)) + 
  geom_density() + 
  labs(x = 'tag', y = 'Density', title = 'Distribution of tags with density plot') +
  theme_bw()

# scatter plot2: check the distribution of rong_tag in tag
ggplot(tag, aes(x = c(1:687374), y = rong_tag)) + 
  geom_point(stat = 'identity') + 
  labs(x = 'Observations', y = 'Tag', title = 'Distribution of tags') +
  theme_bw()



########### relation 1&2 ###############
########################################
r1 <- read.csv('df_re_1.csv')
r2 <- read.csv('df_re_2.csv')
# number of rows = 687374
# length(unique(tag$user_id))
#[1] 16890
# length(unique(tag$rong_tag))
#[1] 37359