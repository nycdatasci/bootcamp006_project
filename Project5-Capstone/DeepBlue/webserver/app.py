from flask import Blueprint
import os
import json
from flask import Flask
import pandas as pd
from flask import render_template
from mf_based_recommender import MFBasedRecommender

main = Blueprint('main', __name__)
base_dir = "/Users/sundeepblue/Bootcamp/allweek/week9/capstone/data/yelp_data/split_business_data_by_city/"

def __get_model_path(city_name):
    model_file_name = "business_recomm_model_for_{}".format(city_name)
    model_full_path = os.path.join(base_dir, city_name, "mf_based_models", model_file_name)
    return model_full_path

# we want to get longitude and latitude information for businesses in this city
def __get_richer_business_info_path(city_name):
    #city_name = "us_charlotte"
    csv_file_name = "{}_business_richer_info.csv".format(city_name)
    full_path = os.path.join(base_dir, city_name, csv_file_name)
    return full_path

def load_richer_biz_info(city_name):
    full_path = __get_richer_business_info_path(city_name)
    richer_biz = pd.read_csv(full_path, sep=',', header=0)
    return richer_biz

# check if a list of keywords match any of the element of category list
# eg: keywords = ["a", "b"], categories = ["c", "b"], return True
def keywords_match_categories(keywords, categories):
    parsed_keywords = []
    for w in keywords:
        pw = w.strip().lower()
        if len(pw) != 0:
            parsed_keywords.append(pw)
    # remove any duplications
    parsed_keywords = list(set(parsed_keywords))
    
    parsed_categories = [c.strip().lower() for c in categories]
    for kw in parsed_keywords:
        for c in parsed_categories:
            # TODO: use nltk NLP to improve accuracy of keywords matching
            # And c could be ["Food"] or ["Fast Food"], we need to differentiate both cases
            sp = c.split(" ")
            if len(sp) == 1: # one-word category, such as ["Food"]
                if kw in c: # keyword=["food"], categories=["seafood"]
                    return True
            else: # multiple-word category, such as ["Fast Food"]
                if kw in sp:
                    return True
    return False

# filter businesses by user-specified keywords
def get_rich_info_of_topk_businesses(richer_biz_info, business_ids, keywords, topk):
    lat_list = []
    lng_list = []
    review_count_list = []
    stars_list = []
    categories_list = []

    if len(keywords) == 0:
        keywords = ["Food"]

    for b in business_ids:
        r = richer_biz_info[richer_biz_info.integer_business_id == b]

        # check the category first
        c = r.categories.values[0] # c now is a stringified list of strings
        c = eval(c)             # convert stringified list back to list
        c = [str(s) for s in c] # convert each unicode string into normal string. Otherwise, js won't recognize them
        #c = "|".join(c)         # now join those strings again
        
        if keywords_match_categories(keywords, c):
            lat_list.append(r.latitude.values[0])
            lng_list.append(r.longitude.values[0])
            review_count_list.append(r.review_count.values[0])
            stars_list.append(r.stars.values[0])
            categories_list.append(c)

            if len(lat_list) == topk:
                break
        
    return (lat_list, lng_list, review_count_list, stars_list, categories_list)


@main.route("/")
def index():
    #return "This is main page!"
    return str(dir(recommender.spark_context))

# a dummy page shows nothing but a map and a streetview
@main.route("/map")
def show_map():
    return render_template("map_with_streetview.html", 
                page_title="Yelper Map Page",
                tb_json=json.dumps({"a": 1, "b": 2}),
                other_data="dummy_data")    

# show social network consisting of users and businesses that those users rated
# in city Madison, USA
@main.route("/network")
def show_network():
    return render_template("network.html", 
                page_title="network",
                other_data="dummy_data")

# http://0.0.0.0:5000/user_id/10081786/city/us_charlotte/top/30/keywords/Restaurants
# http://0.0.0.0:5000/user_id/10033545/city/us_madison/top/300/keywords/food (user is different now!!!!)

@main.route("/user_id/<int:user_id>/city/<string:city_name>/top/<int:topk>/keywords/<string:keywords>", methods=["GET"])
def get_recommended_businesses(user_id, city_name, topk, keywords):
    global current_city_name
    global my_spark_context

    # if user specifies new city, then we need to reload related models/recommender for that city
    if current_city_name != city_name:
        current_city_name = city_name
        load_city_based_models_and_businesses(my_spark_context, current_city_name)
        
    recommended_biz_ids = recommender.recommend_business_for_user(model, user_id) 

    # this step is necessary
    keywords = keywords.split(" ")

    (lat_list, lng_list, review_count_list, stars_list, categories_list) = \
        get_rich_info_of_topk_businesses(richer_biz_info, recommended_biz_ids, keywords, topk)

    if len(lat_list) == 0:
        return "There was no business found! Please use other keywords and retry. Hopefully you'll get something."

    print categories_list
    return render_template("map.html", 
                page_title="Get your recommendations",
                lat_list=lat_list,
                lng_list=lng_list,
                review_count_list=review_count_list,
                stars_list=stars_list,
                #categories_list=json.dumps(categories_list),
                tb_json=json.dumps({"dummy1": 1, "dummy2": 2}),
                other_data="dummy_data")   

def load_city_based_models_and_businesses(spark_context, city_name):
    global recommender
    global richer_biz_info
    global model

    model_path = __get_model_path(city_name)
    recommender = MFBasedRecommender(spark_context, model_path)
    model = recommender.load_mf_model()
    richer_biz_info = load_richer_biz_info(city_name)
    print "City-based models and business data were loaded!"

def create_app(spark_context):    
    global current_city_name
    global my_spark_context # trick here. Recommenders for other cities can be loaded by it

    my_spark_context = spark_context
    current_city_name = "us_charlotte" # default city
    load_city_based_models_and_businesses(spark_context, current_city_name)

    app = Flask(__name__)
    app.register_blueprint(main)
    return app    
