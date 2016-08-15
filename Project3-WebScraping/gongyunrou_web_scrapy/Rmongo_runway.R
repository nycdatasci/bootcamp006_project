library(rmongodb)
library(dplyr)


mongo <- mongo.create()
mongo.is.connected(mongo)
mongo.get.databases(mongo)
mongo.get.database.collections(mongo, db = "runway")
mongo.count(mongo, "runway.dresses")
#tmp <-  mongo.find.one(mongo, "runway.dresses")
#tmp <-  mongo.bson.to.list(tmp)
#names(tmp)
find_all <-  mongo.find.all(mongo, ns = "runway.dresses")
#tmp.df <-  as.data.frame(t(unlist(tmp)), stringsAsFactors = F)
find_all.df <- as.data.frame(Reduce(rbind,t(find_all)))

# remove the string "reviews' in numOfReviews
a = Map(strsplit, find_all.df$numOfReviews, ' ')
a=sapply(a, '[[', 1)
a=sapply(a, '[[', 1)
find_all.df$numOfReviews = a

# assign the "be first to review " into "0"
find_all.df$numOfReviews[find_all.df$numOfReviews=="Be"] <- 0
#assign the "NaN" to "0"
find_all.df$avgRating[find_all.df$avgRating=="NaN"] <- 0
# assign the "list()" to 0
e1 <- c("0","0","0")
find_all.df$fit[find_all.df$fit=="list()"] <- list(e1)

# split the fit into three columns
f <- matrix(data = unlist(find_all.df$fit), ncol = 3, byrow = T,
        dimnames = NULL)
# i <- matrix(data = unlist(find_all.df$img), ncol = 4, byrow = T,
#             dimnames = NULL)
# temp <- sapply(find_all.df$fit, unlist)
find_all.df <- cbind.data.frame(find_all.df, as.data.frame(f))
find_all.df <- dplyr::rename(find_all.df,fit_large = V1,true_to_size=V2,fit_small=V3)


# transfer the data type
item=as.character(unlist(find_all.df$name))
brand=as.factor(unlist(find_all.df$designer))
retailPrice=as.numeric(unlist(find_all.df$retailPrice))
rentalPrice=as.numeric(unlist(find_all.df$rentalPrice))
num_Reviews=as.numeric(find_all.df$numOfReviews)
avgRating=as.numeric(unlist(find_all.df$avgRating))
fit_large=find_all.df$fit_large
true_to_size=find_all.df$true_to_size
fit_small=find_all.df$fit_small

# extract the data for analysis 
data <- cbind.data.frame(item,brand,retailPrice,rentalPrice,num_Reviews,avgRating,
                         fit_large,true_to_size,fit_small)                               


#write.csv(data,file="firstdata.csv",row.names = F)





