######### Now we got two text-aggregated master tables for NLP 
#### One is about negative reviews, the other is about positive reviews
### The standard of positive/negative reviews is different in terms of restaurant category
### For example, 3.5 stars-Fast food restaurant is good in the standard of fast food 
### However, 3.5 stars is the standard of average Thai food restaurant (the average stars for Thai is 4)

#### The following codes are for positive reviews
NLP_positive_business_not_filter=read.csv(file.choose(),stringsAsFactors = F)
 
#### But those businesses that I wouldn't pay attention to
## Those businesses that has average ranking as they should have
#### For positive
Index_remove_positive=which(NLP_positive_business_not_filter$stars_of_business==NLP_positive_business_not_filter$AVG_stars_category_round)
NLP_positive_business_filtered=NLP_positive_business_not_filter[-Index_remove_positive,]

### Let's choose a specific category of restaurant
### The following list is all the restaurant
cuisine_list=c("Afghan",
               "African",
               "American",
               "Arabian",
               "Argentine",
               "Armenian",
               "Asian Fusion",
               "Australian",
               "Austrian",
               "Bangladeshi",
               "Barbeque",
               "Basque",
               "Belgian",
               "Brasseries",
               "Brazilian",
               "Breakfast & Brunch",
               "British",
               "Buffets",
               "Burgers",
               "Burmese",
               "Cafes",
               "Cafeteria",
               "Cajun/Creole",
               "Cambodian",
               "Caribbean",
               "Catalan",
               "Cheesesteaks",
               "Chicken Shop",
               "Chicken Wings",
               "Chinese",
               "Comfort Food",
               "Creperies",
               "Cuban",
               "Czech",
               "Delis",
               "Diners",
               "Dinner Theater",
               "Ethiopian",
               "Fast Food",
               "Filipino",
               "Fish & Chips",
               "Fondue",
               "Food Court",
               "Food Stands",
               "French",
               "Gastropubs",
               "German",
               "Gluten-Free",
               "Greek",
               "Halal",
               "Hawaiian",
               "Himalayan/Nepalese",
               "Hong Kong Style Cafe",
               "Hot Dogs",
               "Hot Pot",
               "Hungarian",
               "Iberian",
               "Indian",
               "Indonesian",
               "Irish",
               "Italian",
               "Japanese",
               "Korean",
               "Kosher",
               "Laotian",
               "Latin American",
               "Live/Raw Food",
               "Malaysian",
               "Mediterranean",
               "Mexican",
               "Middle Eastern",
               "Modern European",
               "Mongolian",
               "Moroccan",
               "New Mexican Cuisine",
               "Nicaraguan",
               "Noodles",
               "Pakistani",
               "Persian/Iranian",
               "Peruvian",
               "Pizza",
               "Polish",
               "Pop-Up Restaurants",
               "Portuguese",
               "Poutineries",
               "Russian",
               "Salad",
               "Sandwiches",
               "Scandinavian",
               "Scottish",
               "Seafood",
               "Singaporean",
               "Slovakian",
               "Soul Food",
               "Soup",
               "Southern",
               "Spanish",
               "Sri Lankan",
               "Steakhouses",
               "Supper Clubs",
               "Sushi Bars",
               "Syrian",
               "Taiwanese",
               "Tapas Bars",
               "Tapas/Small Plates",
               "Tex-Mex",
               "Thai",
               "Turkish",
               "Ukrainian",
               "Uzbek",
               "Vegan",
               "Vegetarian",
               "Vietnamese",
               "Waffles",
               "Bars")

##### Choose a category of Restaurant
NLP_positive_business_filtered_category=subset(NLP_positive_business_filtered,categories=="Italian")
### See how many restaurants are there for the text mining
nrow(NLP_positive_business_filtered_category)

########## LDA
library(LDAvis)
library(devtools)
library(servr)
library(SnowballC)
library(ngram)
library(tau)
library(stringr)
##### For positive reviews
reviews <- NLP_positive_business_filtered_category$text

library(tm)
stop_words <- stopwords("SMART")
reviews <- gsub("'", "", reviews)  # remove apostrophes
reviews <- gsub("[[:punct:]]", " ", reviews)  # replace punctuation with space
reviews <- gsub("[[:cntrl:]]", " ", reviews)  # replace control characters with space
reviews <- gsub("^[[:space:]]+", "", reviews) # remove whitespace at beginning of documents
reviews <- gsub("[[:space:]]+$", "", reviews) # remove whitespace at end of documents
reviews <- tolower(reviews)  # force to lowercase
reviews <- removeNumbers(reviews)
reviews=wordStem(reviews,language = "en")

###ASCII
reviews=remove_stopwords(reviews, stop_words, lines = T)
reviews= stripWhitespace(reviews)
reviews=iconv(reviews,to="ASCII",sub="")
# doc.list <- strsplit(reviews, "[[:space:]]+")


#### After removing stopwords_ I need to fill in some words for the algorithm to run

##### Define a new function to count the words
nwords <- function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}
### For those reviews that have less than 3 words, replace it "are very good"
reviews[which(nwords(reviews)<3)]="are very good"
doc.list=list()
for (i in 1:nrow(NLP_positive_business_filtered_category)){
  
  a=ngram::ngram_asweka(reviews[i], min=2, max=3)
  doc.list[[length(doc.list)+1]] =a
}

term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 4
term.table <- term.table[!del]
vocab <- names(term.table)















get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

D <- length(documents)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # f

K <- 4
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
library(doMC)
registerDoMC(cores = 4)
fit_Italian_Positive <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop

theta <- t(apply(fit_Italian_Positive$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit_Italian_Positive$topics) + eta, 2, function(x) x/sum(x)))


RestaurantReview_Italian_Positive <- list(phi = phi,
                         theta = theta,
                         doc.length = doc.length,
                         vocab = vocab,
                         term.frequency = term.frequency)


library(LDAvis)
RestaurantReview_Italian_Positive$vocab[136]="Restaurant"
# create the JSON object to feed the visualization:
json_Italian_Positive <- createJSON(phi = RestaurantReview_Italian_Positive$phi, 
                   theta = RestaurantReview_Italian_Positive$theta, 
                   doc.length = RestaurantReview_Italian_Positive$doc.length, 
                   vocab = RestaurantReview_Italian_Positive$vocab, 
                   term.frequency = RestaurantReview_Italian_Positive$term.frequency)

serVis(json_Italian_Positive, out.dir = 'vis', open.browser = T)


save("fit_Italian_Positive","RestaurantReview_Italian_Positive","json_Italian_Positive",file="Italian_topic_model_Positive_4.Rdata")
