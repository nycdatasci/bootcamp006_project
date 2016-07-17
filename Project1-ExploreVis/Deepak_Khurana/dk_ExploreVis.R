#Loading data from YELP Dataset Challenge (Round 7) 

lbrary(jsonlite)                  # library to read data provided in .json file format

checkin = stream_in(file("/Users/dk1306/Downloads/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_checkin.json"))   # read checkin data to checkin df 
business = stream_in(file("/Users/dk1306/Downloads/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json")) # read business data to business df
tip = stream_in(file("/Users/dk1306/Downloads/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_tip.json"))           # read tip data to tip df
user = stream_in(file("/Users/dk1306/Downloads/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_user.json"))         # read user data to user df
review = stream_in(file("/Users/dk1306/Downloads/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json"))     # read review data to review df




