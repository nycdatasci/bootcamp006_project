import os
#from pyspark import SparkContext, SparkConf
from pyspark.mllib.recommendation import MatrixFactorizationModel

class MFBasedRecommender:

    def __init__(self, spark_context, model_path):
        self.spark_context = spark_context
        self.model_path = model_path

    def load_mf_model(self):
        loaded_model = MatrixFactorizationModel.load(self.spark_context, self.model_path)
        return loaded_model
        
    def recommend_business_for_user(self, model, user_id, topk=1000):
        ratings = model.recommendProducts(user_id, topk)
        return [int(r.product) for r in ratings]

# def load_model(spark_context, recommender):
#     base_dir = "/Users/sundeepblue/Bootcamp/allweek/week9/capstone/data/yelp_data/split_business_data_by_city/"
#     city_name = "us_charlotte"
#     model_file_name = "business_recomm_model_for_{}".format(city_name)
#     model_full_path = os.path.join(base_dir, city_name, "mf_based_models", model_file_name)
#     model = recommender.load_mf_model(spark_context, model_full_path)
#     return model

# def recommend_for_one_user(spark_context, rec):
#     model = load_model(spark_context, rec)
#     user_id = 10336396
#     print rec.recommend_business_for_user(model, user_id, topk=50)

# appName = "Yelper Recommendation System Trainer"
# conf = SparkConf().setAppName(appName).setMaster("local")
# spark_context = SparkContext(conf=conf)
# rec = MFBasedRecommender()
# recommend_for_one_user(spark_context, rec)



