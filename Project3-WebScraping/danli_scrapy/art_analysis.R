library(dplyr)
library(ggplot2)
library(tm)
library(SnowballC) 
library(wordcloud)
library(d3heatmap)

artist <- read.csv('artist_cleaned.csv', header = TRUE, stringsAsFactors = FALSE, na.strings=c("","NA"))
################### Imputation ######################
# First thing caught my eyes: a lot of missing values for genre
sum(is.na(artist$genre))/nrow(artist)
# 68% observation's genre info is missing.
nrow(artist) - sum(is.na(artist$genre))
# But we still have 2885 valid entries
# Maybe we can develop a model to predict an artist's genre?

sum(is.na(artist$nationality)) # 219
sum(is.na(artist$nationality) & is.na(artist$genre) == FALSE) 
# Only 15, so we can delete these obs w/o nationality info,
# It won't massively impact our training dataset.
artist <- filter(artist, is.na(artist$nationality) == FALSE)

sum(is.na(artist$medium)) #1395
# Replace all the missing medium with Painter
artist$medium[is.na(artist$medium)] <- "Painter"

sum(is.na(artist$born)) #109
sum(is.na(artist$born) & is.na(artist$genre) == FALSE)
# Only 1, so we can delete these obs w/o born info,
# It won't massively impact our training dataset.
artist <- filter(artist, is.na(artist$born) == FALSE)

# Replace the born year written in century to generalized year
for(i in 1:nrow(artist)) {
  if (artist$born[i] <= 100 & is.na(artist$born[i]) == FALSE) {
    artist$born[i] <- artist$born[i]*100
  }
}

# Create a new column century which represent which century the artist was born
artist$century <- 1:nrow(artist)
for(i in 1:nrow(artist)) {
  if (is.na(artist$born[i])) {
    artist$century[i] <-   NA
  } else {
    artist$century[i] <- (artist$born[i] %/% 100) + 1
  }
}

sum(is.na(artist$museums)) # 398
sum(is.na(artist$museums) & is.na(artist$genre) == FALSE) 
# 69. We can't delete them since we want to keep other information, so here we replace NA with Unknown(aka there are no museums collecting this artist's work or we don't know.)
artist$museums[is.na(artist$museums)] <- "Unknown"

# Clean unreasonable firstnames
artist$firstName[artist$firstName  == "Sir"] <- NA
artist$firstName[artist$firstName  == "Master"] <- NA
artist$firstName[artist$firstName  == "The"] <- NA

# Prepare the museums variable
# First we need to standardize the museum names
for(i in 1:nrow(artist)){
  to_replace <- paste0(artist$artist[i], ' at the ')
  artist$museums[i] <-  gsub(to_replace, '', artist$museums[i])
}

########### Part 1 - Some interesting ranks ############
# Which century contributed to us the most artists?
century_rank <- arrange(as.data.frame(table(artist$century)), desc(Freq))
p = ggplot(century_rank, aes(x=reorder(century_rank$Var1, desc(century_rank$Freq)), y=century_rank$Freq)) + geom_bar(stat = 'identity', aes(fill= century_rank$Freq)) + xlab('Century') + ylab('Number of Artists') + ggtitle('Number of Artists by Century')
p$labels$fill <- "Number of Artists"
p

# Which country contributed to us the most artists?
nation_rank <- arrange(as.data.frame(table(artist$nationality)), desc(Freq))
nation_rank_ <- head(nation_rank,20)
p1 = ggplot(nation_rank_, aes(x=reorder(nation_rank_$Var1, desc(nation_rank_$Freq)), y=nation_rank_$Freq)) + geom_bar(stat = 'identity', aes(fill= nation_rank_$Freq)) + xlab('Nation') + ylab('Number of Artists') + ggtitle('Number of Artists by Nationality (top 20)') + theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 10))
p1$labels$fill <- "Number of Artists"
p1

# Which genre do we consider main stream?
genre_rank <- arrange(as.data.frame(table(artist$genre)), desc(Freq))
p2 = ggplot(genre_rank, aes(x=reorder(genre_rank$Var1, desc(genre_rank$Freq)), y=genre_rank$Freq)) + geom_bar(stat = 'identity', aes(fill= genre_rank$Freq)) + xlab('Genre') + ylab('Number of Artists') + ggtitle('Number of Artists by Genre') + theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 10))
p2$labels$fill <- "Number of Artists"
p2

# Which medium are the most popular ones in the realm of art?
medium_rank <- arrange(as.data.frame(table(artist$medium)), desc(Freq))
p3 = ggplot(medium_rank, aes(x=reorder(medium_rank$Var1, desc(medium_rank$Freq)), y=medium_rank$Freq)) + geom_bar(stat = 'identity', aes(fill= medium_rank$Freq)) + xlab('Medium') + ylab('Number of Artists') + ggtitle('Number of Artists by Medium') + theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 10))
p3$labels$fill <- "Number of Artists"
p3

########### Part 2 - Wordcloud for fun ############
# Wordcloud of artist name
nameCorpus <- Corpus(VectorSource(artist$firstName))
nameCorpus <- tm_map(nameCorpus, PlainTextDocument)
wordcloud(nameCorpus, max.words = 100, random.order = FALSE)

# Wordcloud of museum name, just for fun, not really noteworthy
museumCorpus <- Corpus(VectorSource(artist$museums))
museumCorpus <- tm_map(museumCorpus, PlainTextDocument)
museumCorpus <- tm_map(museumCorpus, removePunctuation)
museumCorpus <- tm_map(museumCorpus, stemDocument)
wordcloud(museumCorpus, max.words = 200, random.order = FALSE)

########### Part 3 - Heatmap of art history ############
# Heatmap of the number of artist by nationality and genre
heat_data <- as.data.frame.matrix(table(artist$genre, artist$nationality))
d3heatmap(heat_data, scale = "none", dendrogram = "none", color = "Blues")

# Heatmap of the number of artist by century and genre
heat_data1 <- as.data.frame.matrix(table(artist$century, artist$genre))
d3heatmap(heat_data1, scale = "none", dendrogram = "none", color = "Blues")

# Heatmap of the number of artist by century and nationality 
heat_data2 <- as.data.frame.matrix(table(artist$century, artist$nationality))
d3heatmap(heat_data2, scale = "none", dendrogram = "none", color = "Blues")

############## Model- Predict artist genre #################
# First delete all records before 11th century 
# cuz they have no genre info at all.
artist_genre <- filter(artist, artist$century >= 11)
artist_genre <- select(artist_genre, -c(X, X_id, X.1, details, death))
write.csv(artist_genre, file = "artist_final.csv")
artist_genre <- select(artist_genre, -c(artist, firstName, century))
artist_genre$genre <- as.factor(artist_genre$genre)
artist_genre$nationality <- as.factor(artist_genre$nationality)
artist_genre$medium <- as.factor(artist_genre$medium)

# Split the dataset into one with valid genre info and the other one with genre info missing
artist_no_genre <- filter(artist_genre, is.na(artist_genre$genre) == TRUE)
artist_genre <- filter(artist_genre, is.na(artist_genre$genre) == FALSE)

# Create train & test datasets
set.seed(0)
train <-  sample(1:nrow(artist_genre), 8*nrow(artist_genre)/10)
genre.test <-  artist_genre[-train, ]
res.test <- genre.test$genre

# train_sparse <- sparse.model.matrix(~.,artist_genre[,-1])
# test_sparse <- sparse.model.matrix(~.,genre.test[,-c(1,2)])
# dim(train_sparse)
# fit <- glmnet(train_sparse,artist_genre[,2],family="multinomial")
# 
# pred <- predict(fit, test_sparse,type="response", s=cv$lambda.min)
# fit <- glmnet(train_sparse,train[,])
# pred <- predict(fit, test_sparse, type="class")

# Create a function that switch artist & museums as values and keys
create_idx <- function(key, vals) {
  for(val in vals) {
    if(val %in% names(my_index)) {
      my_index[[val]] <<- unique(c(my_index[[val]], key))
    } else {
      my_index[[val]] <<- key
    }
  }
}

# Apply the function to our dataset, create a list with genres as the keys and artists as the value
my_index <- list()
for(i in 1:nrow(artist_genre)) {
  key = artist_genre$genre[i]
  vals = unlist(strsplit(artist_genre$museums[i], ", "))
  create_idx(key, vals)
}

tree.genre = tree(genre ~ .-museums, split = "gini", data = artist_genre)
tree.genre
# There are 48 unique genres, 68 unique nationalities, 34 unique mediums and I can not pass it into decision tree.
genre.pred = predict(tree.genre, genre.test[ ,-1], type = "class")

