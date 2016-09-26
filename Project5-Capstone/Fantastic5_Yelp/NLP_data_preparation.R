##### Now we are starting from NLP_table

### We need to get a average stars of each of the categories
library(dplyr)
load("NLP.RData")
Intermediate_table=NLP_table %>%
  group_by(categories) %>%
  summarise(AVG_stars_category=mean(stars_of_business))
#### round to nearest half(0.5)
Intermediate_table$AVG_stars_category_round=round(Intermediate_table$AVG_stars_category/0.5)*0.5


### group the A and NLP original
Revise_NLP_table=NLP_table %>%
  left_join(Intermediate_table,by=c("categories"="categories"))

#### Get the Index of self-defined positive reviews
Index_Positive=which(Revise_NLP_table$stars_of_individual_review>Revise_NLP_table$AVG_stars_category_round)
Revise_NLP_table_positive=Revise_NLP_table[Index_Positive,]
#### Get the Index of self-defined negative reviews
Index_Negative=which(Revise_NLP_table$stars_of_individual_review<Revise_NLP_table$AVG_stars_category_round)
Revise_NLP_table_negative=Revise_NLP_table[Index_Negative,]



# #### what if I try a step further to remove those reviews that I wouldn't pay attention to 
# ## (businesses that has average ranking as they should have)
# Index_remove_negative=which(Revise_NLP_table_negative$stars_of_business==Revise_NLP_table_negative$AVG_stars_category_round)
# Revise_NLP_table_negative1=Revise_NLP_table_negative[-Index_remove_negative,]
# Index_remove_positive=which(Revise_NLP_table_positive$stars_of_business==Revise_NLP_table_positive$AVG_stars_category_round)
# Revise_NLP_table_positive1=Revise_NLP_table_positive[-Index_remove_positive,]
# 
# ### output
# write.csv(Revise_NLP_table_positive1,"Revise_NLP_table_positive1.csv")
# write.csv(Revise_NLP_table_negative1,"Revise_NLP_table_negative1.csv")
