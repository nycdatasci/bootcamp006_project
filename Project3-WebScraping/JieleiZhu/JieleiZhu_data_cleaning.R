setwd('~/Google Drive/DataScienceAcademy/PROJECTS/#3Web Scraping')
library(dplyr)

#---------- Load data
my_read_csv = function (filename, 
                        header = T, 
                        colClasses = c('factor', 'character', 'character', 'character', 'character', 'character')) {
  return(read.csv(filename, header = header, colClasses = colClasses))
}

baby_raw = my_read_csv('Dealmoon_data/Baby.csv')
beauty_raw = my_read_csv('Dealmoon_data/Beauty.csv')
clothing_raw = my_read_csv('Dealmoon_data/Clothing.csv')
electronics_raw = my_read_csv('Dealmoon_data/Electronics.csv')
finance_raw = my_read_csv('Dealmoon_data/Finance.csv')
home_raw = my_read_csv('Dealmoon_data/Home.csv')
nutrition_raw = my_read_csv('Dealmoon_data/Nutrition.csv')
travel_raw = my_read_csv('Dealmoon_data/Travel.csv')


#---------- Merge all data files into one .csv file
deal_data = Reduce(rbind, list(baby_raw, beauty_raw, clothing_raw, electronics_raw, finance_raw, home_raw, nutrition_raw, travel_raw))
num_rows = nrow(deal_data)

#---------- Get rid of '+' in '999+' and convert 'NumOfComments' and "NumOfBookmarks' to integer type

replace999 = function (col, replace_value, by_value) {
  return(replace(col, which(col == replace_value), by_value))
}

deal_data$NumberOfComments = replace999(deal_data$NumberOfComments, '999+', '999')
deal_data$NumberOfBookmarks = replace999(deal_data$NumberOfBookmarks, '999+', '999')

deal_data$NumberOfComments = sapply(deal_data$NumberOfComments, as.integer)
deal_data$NumberOfBookmarks = sapply(deal_data$NumberOfBookmarks, as.integer)

str(deal_data)


#----------- Extract out store names

# find common patterns of store names
deal_data$Title[setdiff(1:num_rows, grep(' @ ', deal_data$Title))[1]]
deal_data$Description[setdiff(1:num_rows, grep(' @ ', deal_data$Title))[1]]
indices = setdiff(1:num_rows, grep(' @ ', deal_data$Title))
deal_data$Description[grepl('^Amazon.com ', deal_data$Description[indices]) == F]
######### Pattern: store names are always included in 'Description' right before 'offers'

# confirm 'offers' exists in all 'Descriptions'
length(grep(' offers', deal_data$Description))
deal_data$Description[setdiff(1:num_rows, grep('offers', deal_data$Description))[22]]
######### Nope(exists in 45314/46836 deals); 

# confirm 'offers' exists in all 'Description' where the store name is not included in 'Title'
no_at = setdiff(1:num_rows, grep('@', deal_data$Title))
length(no_at)
length(grep(' offers', deal_data$Description[no_at]))
######### Not the same: 27148 vs. 26173 (diff: 975)


# sample deals where store name is not included in 'Title' AND 'offers' does not exist in 'Description'
where_store_indices = setdiff(1:num_rows, union(grep('@', deal_data$Title), grep(' offers', deal_data$Description)))
sample(deal_data$Description[where_store_indices], 10)
######### No obvious pattern


# visualize distribution of categories of these deals 
barplot(table(deal_data$Category[where_store_indices]))
######### Present in all categories with the most in 'Baby' and 'Travel'


# remove these deals from 'deal_data'
deal_data = deal_data[-where_store_indices, ]
num_rows = nrow(deal_data)


# extract store names
deal_data$Store = c('')
#1. get rid of all '.com' in 'Title' and 'Description'
length(grep('.com', deal_data$Title)) ###### originally has 3407 '.com' in 'Title'
deal_data$Title = gsub('.com', '', deal_data$Title)
length(grep('.com', deal_data$Title)) ###### now 0

length(grep('.com', deal_data$Description)) ###### originally has 18921 '.com' in 'Title'
deal_data$Description = gsub('.com', '', deal_data$Description)
length(grep('.com', deal_data$Description)) ###### now 0

# #2. find deals where store names are saved in 'Title'
# store_title_indices = grep('@', deal_data$Title)
# deal_data$Store[store_title_indices] = sub("(^.* *@)([A-z0-9 \\']+)([[^'-][:punct:]]*.*$)", '\\2', deal_data$Title[store_title_indices])
# # check accuracy
# sample(deal_data$Store[store_title_indices], 20)
# #3. find deals where store names are saved in 'Description'
# store_description_indices = setdiff(grep(' offers', deal_data$Description), store_title_indices)
# deal_data$Store[store_description_indices] = Map(sub, "([A-z0-9]+[[:punct:]]+ )*([A-z0-9 '-]+)( *offers.*$)", '\\2', Map(grep,' offers', Map(function(x) x[[1]], Map(strsplit, deal_data$Description[store_description_indices], '\n')), value = T))

store_description_indices = grep(' offers', deal_data$Description)
deal_data$Store[store_description_indices] = sub("([A-z0-9]+[[:punct:]]+ )*([A-z0-9 '-]+)( *offers.*$)", '\\2', grep(' offers', sapply(strsplit(deal_data$Description[store_description_indices], '\n'), '[[', 1), value = T))

store_title_indices = setdiff(grep('@', deal_data$Title), store_description_indices)
deal_data$Store[store_title_indices] = sub("(^.* *@ *)([A-z0-9 '-]+)([[^'-][:punct:]]*.*$)*", '\\2', deal_data$Title[store_title_indices])

#4. if the 'Store' field contains a list, save only the first element, which is the true label
SaveFirst = function(x) {
  if(length(x) > 1) return(x[1])
  else return(x)
}
deal_data$Store = sapply(deal_data$Store, SaveFirst)
# check accuracy
sample(deal_data$Store, 20)
# debugging
deal_data$Description[which(deal_data$Store == 'SSENSE ')]
# trim off leading and trailing white spaces
deal_data$Store = trimws(deal_data$Store)

# strip off any non-alphanumeric symbols in store names
deal_data$Store = gsub("[^[:alnum:]]|^ ", "", deal_data$Store)

# change all store names to uppercase
deal_data$Store = toupper(deal_data$Store)

# replace all store names that contain "Amazon" with "Amazon"
deal_data$Store[grep('AMAZON', deal_data$Store)] = 'AMAZON'

# replace all store names that contain "BESTBUY" with "BESTBUY"
deal_data$Store[grep("BESTBUY", deal_data$Store)] = "BESTBUY"

# replace all store names that contain "GNC" with "GNC"
deal_data$Store[grep("GNC", deal_data$Store)] = "GNC"

# strip away 'TODAYONLY#' from all store names
deal_data$Store = sub('^TODAYONLY[0-9]*', '', deal_data$Store)

# strip away 'TODAY#' from all store names
deal_data$Store = sub('^TODAY[0-9]*', '', deal_data$Store)

# remove 'MULTIPLESTORE' and 'MULTIPLESTORES'
deal_data = deal_data[-grep('MULTIPLESTORE', deal_data$Store), ]

# remove 'MULTIPLESTORE' and 'VARIOUSSTORES'
deal_data = deal_data[-grep('VARIOUSSTORES', deal_data$Store), ]



#--------- Clean 'Time'
# Change all 'Time' with unit other than 'day' to '0 day'
deal_data$PostedTime[setdiff(setdiff(1:num_rows, grep('day', deal_data$PostedTime)), grep('days', deal_data$PostedTime))] = '0 day'

# strip off 'day' and 'days', so the entire column is numeric
deal_data$PostedTime = sub('[^[:digit:]]+', '', deal_data$PostedTime)

# change column class to integer
deal_data$PostedTime = as.integer(deal_data$PostedTime)


#--------- Analysis
# combine 'NumberOfComments' and 'NumberOfBookmarks' into one column 'Popularity'
deal_data$Popularity = deal_data$NumberOfComments + deal_data$NumberOfBookmarks



#--------- Back up cleaned data so far
write.csv(deal_data, 'Dealmoon_data/Deal_data2.csv', row.names=F)

my_read_csv2 = function (filename, 
                        header = T, 
                        colClasses = c('factor', 'character', 'character', 'integer', 'integer', 'integer', 'character', 'integer')) {
  return(read.csv(filename, header = header, colClasses = colClasses))
}

deal_data = my_read_csv2('Dealmoon_data/Deal_data2.csv')




popularity_overall = deal_data %>% group_by(Category, Store) %>% summarise(NumDeals = n(), Popularity = sum(Popularity))

popularity_by_category = deal_data %>% group_by(Category) %>% summarise(Total = sum(Popularity))

popularity_data = merge(popularity_overall, popularity_by_category, by = 'Category')

popularity_data$PPD = popularity_data$Popularity / popularity_data$NumDeals

# Rank deals by popularity in each category 
Top_5_deal = deal_data %>% group_by(Category) %>% top_n(5, Popularity)

# Rank stores by total number of deals
Top_5_freq = popularity_overall %>% top_n(5, NumDeals)

# Rank Stores by 'popularity per deal(PPD)'
Top_5_PPD = popularity_data %>% group_by(Category) %>% filter(NumDeals >= 5) %>% top_n(5, PPD)

# 





