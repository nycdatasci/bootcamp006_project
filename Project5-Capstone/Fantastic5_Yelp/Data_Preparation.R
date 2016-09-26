#### 
library(data.table)
library(stringr)
library(dplyr)
library(jsonlite)


#### Read the five tables
Business_table=fread("yelp_academic_dataset_business.csv",stringsAsFactors = F)
Review_table=read.csv("yelp_academic_dataset_review.csv",stringsAsFactors = F)
Tips_table=fread("yelp_academic_dataset_tip.csv",stringsAsFactors = F)
User_table=fread("yelp_academic_dataset_user.csv",stringsAsFactors = F)
Check_in_table=fread("yelp_academic_dataset_checkin.csv",stringsAsFactors = F)

#### Pattern Matching
Business_table_AZ=Business_table[state=="AZ"]
pattern="Restaurants"
strings=Business_table_AZ$categories


### get the indexes of restaurants
Restaurants_Index=which(str_detect(strings,pattern))
Business_table_AZ_Res=Business_table_AZ[Restaurants_Index,]
Business_ID=Business_table_AZ_Res$business_id

#### Use Business Id as a baseline

### For Tips Tables
Tips_table_Res=Tips_table  %>%
             filter(business_id %in% Business_ID)
### For Review Tables
Review_table_Res =Review_table %>%
            filter(business_id %in% Business_ID)
### For Check_in Tables
Check_in_Res= Check_in_table %>%
             filter(business_id %in% Business_ID)
### For User_table, it's different (no business id)
User_table_reduced=User_table[which(User_table$user_id %in% Review_table_Res$user_id)]

#### Export as csv file
write.csv(Business_table_AZ_Res,"Business_Reduced.csv")


save("Business_table_AZ_Res","Check_in_Res",
     "Review_table_Res","Tips_table_Res","User_table_reduced",
     "pattern","Business_ID","Restaurants_Index","strings",
     file="Five_reduced_tables.Rdata")

###### This is to generate a json file for Chales
json_for_Chales=toJSON(Review_table_Res,pretty = T)
write(json_for_Chales,file="review_AZ_Resta.json")

### remove those redundance
rm(Business_table)
rm(Review_table)
rm(Check_in_table)
rm(Tips_table)
rm(User_table)
