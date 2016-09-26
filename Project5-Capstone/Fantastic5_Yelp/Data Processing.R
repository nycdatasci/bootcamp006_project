### Load libraries
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)

### Load the data from the step: data preparation
load("Five_reduced_tables.Rdata")


#### There are two paths here
### path1: NLP (review table / business table needed, common key=business_id )
### path2: prediction (review/business/check_in needed)
#### Since the table for path1 could be generated along the way of path1,
### the table grouping is following the path1


##### (1)Check_in table incorporated in business table
### final table "business_checkin_join"
Bus_ID=Check_in_Res$business_id
Check_in_table_rearrange=cbind.data.frame(Bus_ID,Check_in_Res[,c(-15,-106)])

check_in_table_reshape=gather(Check_in_table_rearrange,Time,Count,-Bus_ID)

Busy_hour= check_in_table_reshape %>%
  group_by(Time) %>%
  summarise(check_in_number=sum(Count,na.rm = T))
write.csv(Busy_hour,"Busy_hour.csv")

Popular_Res= check_in_table_reshape%>%
  group_by(Bus_ID) %>% 
  summarise(check_in_number=sum(Count,na.rm=T))
write.csv(Popular_Res,"Popular.csv")

rm(Bus_ID)
business_checkin_join=left_join(Business_table_AZ_Res,Popular_Res,by=c("business_id"="Bus_ID"))

#####(2)Review table incorporated in large business table
#### final table:review_business_group
### path1 table generated along the way
#### Assigning categories for business by cuisines
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

##### use a for loop to replace the messy categories with a well-defined category for each business
for (cuisine in cuisine_list){
  pattern=cuisine
  strings=business_checkin_join$categories
  Index=which(str_detect(strings,pattern))
  business_checkin_join[Index,21]=cuisine
}

###### After grouping with business table
##### reviews could be extracted and analyzed independently
review_business_group=business_checkin_join %>%
  left_join(Review_table_Res,by=c("business_id"="business_id"))
### To distinguish the star of a restaurant and a review
colnames(review_business_group)[62]="stars_of_business"
colnames(review_business_group)[105]="stars_of_individual_review"
write.csv(Review_table_Res_slimed,"Review_table_Res_slimed.csv")

#### NLP analysis
NLP_table= review_business_group %>%
  select(business_id,text,stars_of_individual_review,categories)
save("NLP_table",file="NLP.RData")
write.csv(NLP_table,"NLP_table.csv")

### NLP positive
NLP_table_Negative=NLP_table %>% 
  filter(stars_of_individual_review<=3) %>%
  select(-stars_of_individual_review)
NLP_table_Positive=NLP_table %>%
  filter(stars_of_individual_review>3) %>%
  select(-stars_of_individual_review)
write.csv(NLP_table_Positive,"NLP_table_Positive.csv")
write.csv(NLP_table_Negative,"NLP_table_Negative.csv")

#### Remove those unimportant variables
column_index_remove=c(-107,-39,-24,-16,-45,-64,-100,-101,-106,-94)
Review_table_Res_slimed=review_business_group[,column_index_remove]



# #####(3)User table incorporated in large business table
# #### final table: User_table_reduced_grouped
# User_table_reduced_grouped=User_table_reduced %>%
#   select(2:8,9:14,17:23) %>%
#   right_join(review_business_group,by=c("user_id"="user_id"))




#####(4)Tips table incorporated in large business table
#### final table: 
# Tips_table_business=Tips_table_Res %>%
#   select(2,3) %>%
#   left_join(User_table_reduced_grouped,by=c("business_id"="business_id"))
# ####To distinguish the text of a tip and a review
# colnames(Tips_table_business)[1]="text from a tip"
# colnames(Tips_table_business)[122]="text_individual_review"