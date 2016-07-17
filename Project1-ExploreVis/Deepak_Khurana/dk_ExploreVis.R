#Loading data from YELP Dataset Challenge (Round 7) 

library(jsonlite)                  # library to read data provided in .json file format

setwd('~/Downloads/yelp_dataset_challenge_academic_dataset/')

#checkin  = stream_in(file("yelp_academic_dataset_checkin.json"))   # read checkin data to checkin df 
business = stream_in(file("yelp_academic_dataset_business.json"))   # read business data to business df
#tip      = stream_in(file("yelp_academic_dataset_tip.json"))       # read tip data to tip df
#user     = stream_in(file("yelp_academic_dataset_user.json"))      # read user data to user df
#review   = stream_in(file("yelp_academic_dataset_review.json"))    # read review data to review df



#checkin_flat  = flatten(checkin)
business_flat = flatten(business)
#tip_flat      = flatten(tip)
#user_flat     = flatten(user)
#review_flat   = flatten(review)


library(tibble)

#checkin_tbl   = as_data_frame(checkin_flat)
business_tbl  = as_data_frame(business_flat)
#tip_tbl       = as_data_frame(tip_flat)
#user_tbl      = as_data_frame(user_flat)
#review_tbl    = as_data_frame(review_flat)




