# David Richard Steinmetz
# davidsteinmetz@gmail.com

# Supervisor function which calls functions to:
# 1. Get the recommendations from the graphlab model
# 2. Extract the cluster data from the graphlab model recommendations
# 3. Get data about businesses on the map from Yelp API
# 4. Classify the businesses using KNN to clusters based on the pre-run K-means model
# 5. Sort the businesses by cluster and rating to be able to rec good restaurants
# 6. Generate a list of recommended restaurants based on knowledge of model recs

from Cluster import *
from GLModel import gl_model
import numpy as np

user_id = ['8J4IIYcqBlFch8T90N923A', 'QXwSbE7fwXlBROW6E_FcqQ']
num_rec = 40
bounding_box = {'sw_latitude': 37.900000,
                'sw_longitude': -122.500000,
                'ne_latitude': 37.788022,
                'ne_longitude': -122.399797}
search_params = {
    'term': 'burgers,chinese,japanese,mexican,seafood',
    'lang': 'en'
}

def supervisor(user_id, num_rec, bounding_box, search_params):
    # --- Define constants
    num_clust = 5  # Number of clusters from the recommended restaurants to take for prediction

    # --- Test structures
    np.random.seed(26) #!!!!!!!!!!!!!!
    test_biz_cluster = pd.Series(np.random.randint(1, 25, num_rec * len(user_id))) #!!!!!!!!!!!!!!
    # test_model_recs = pd.DataFrame({'cluster': np.random.randint(1, 25, 10)})
    test_model_clusters = np.random.randint(1, 25, num_rec * len(user_id)) #!!!!!!!!!!!!!!

    # --- Collaborative filtering recommendations (1 + 2)
    bus_types = search_params['term'].split(',')        # Create list of business types
    # model_recs = test_model_recs                      # Get model recommendations
    model_recs = gl_model(user_id, num_rec, bus_types)  # Get model recs
    # print 'model_recs\n', type(model_recs)
    # print model_recs

    # Assign clusters to graphlab model recs
    # biz_cluster = load_pickle('biz4.pkl')  # Load business data
    # resto = pd.merge(model_recs, biz_cluster, on=['business_id'])  # Merge to get data to predict
    # Centroids = load_pickle('Centroids.pkl')  # Load centroids from DBS model
    # old_resto = dbscan_predict(Centroids, resto[['stars','PriceRange','LogReviewCnt']])  # Assign clusters (classify)
    # clust_to_rec_from = get_unique_model_clusters(old_resto, num_clust)   # Get unique clusters #!!!!!!!!!!!!!!
    # print 'clust_to_rec_from\n', clust_to_rec_from
    clust_to_rec_from = test_model_clusters  # Get clusters for model recs #!!!!!!!!!!!!!!

    # --- Classify and sort (3 + 4 + 5)
    biz = get_businesses_on_map(bounding_box, search_params)  # Get businesses on map
    # biz['cluster'] = assign_map_clusters(biz)  # Classify biz on map to clusters (DBS) #!!!!!!!!!!!!!!
    # print 'biz[cluster]\n', biz['cluster']
    biz['cluster'] = test_biz_cluster  # Classify biz on map to clusters (DBS) #!!!!!!!!!!!!!!
    sorted_biz = cluster_rating_sort(biz)  # Sort biz by cluster and rating

    # --- Generate recommendations in new locale (6)
    map_recs = gen_map_recs(sorted_biz, clust_to_rec_from)  # Generate recs from biz on map
    js_map_recs = map_recs.to_json()
    return js_map_recs


# Test supervisor function
supervisor(user_id, num_rec, bounding_box, search_params)
