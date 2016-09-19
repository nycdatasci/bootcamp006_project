import os
import json
from rating_data_utils import *
from pyspark.sql import Row
from pyspark.sql import SQLContext
from pyspark import SparkContext, SparkConf


appName = "Yelper Recommendation System"
conf = SparkConf().setAppName(appName).setMaster("local")
sc = SparkContext(conf=conf)
# sc.setLogLevel("WARN") # config log level to make console less verbose
sqlContext = SQLContext(sc)

userid_to_index_map = load_userid_to_index_map()
bizid_to_index_map = load_bizid_to_index_map()

parsed_ratingdata_all_cities = build_rating_df_for_all_cities(sc, sqlContext, userid_to_index_map, bizid_to_index_map)

base_dir = "/Users/sundeepblue/Bootcamp/allweek/week9/capstone/data/yelp_data/split_business_data_by_city/"

# process all major cities
us_cities = [
                ("NC", "us_charlotte"), 
                ("NV", "us_lasvegas"), 
                ("WI", "us_madison"),
                ("AZ", "us_phoenix"), 
                ("PA", "us_pittsburgh"), 
                ("IL", "us_urbana_champaign")
            ]
canada_cities = [("QC", "canada_montreal")]
germany_cities = [("BW", "germany_karlsruhe")]
uk_cities = [("EDH", "uk_edinburgh")]

cities = us_cities + canada_cities + germany_cities + uk_cities
city_names = [p[1] for p in cities]

for city_name in city_names:
    builder = RatingDataBuilderForCity(city_name, base_dir, sc)
    builder.process_one_city(bizid_to_index_map, parsed_ratingdata_all_cities)

print "The rating data of all {} cities were successfully processed!".format(len(city_names))


