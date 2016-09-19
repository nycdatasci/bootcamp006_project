# David Richard Steinmetz
# NYCDSA - Capstone Project


def search_yelp(secret_file, location, search_parameters):
    # Extract Yelp Data through the API
    import io
    import json
    from yelp.client import Client
    from yelp.oauth1_authenticator import Oauth1Authenticator

    # read API keys
    with io.open(secret_file) as cred:  # Auth in file not shared on github
        creds = json.load(cred)
        auth = Oauth1Authenticator(**creds)
        client = Client(auth)

    # There are three ways to query the Search API: search, search_by_bounding_box, and
    # search_by_coordinates. For each of these methods, additional parameters are optional.
    # The full list of parameters can be found on the Search API Documentation.
    # https://github.com/Yelp/yelp-python

    # search_by_bounding_box takes a southwest latitude/longitude and a northeast
    # latitude/longitude as the location boundary
    response = client.search_by_bounding_box(
        location['sw_latitude'],
        location['sw_longitude'],
        location['ne_latitude'],
        location['ne_longitude'],
        **search_parameters
    )

    return response


# Function to extract locations of search results
def get_response_coords(response):
    coord = []

    for i in range(len(response.businesses)):
        latitude = response.businesses[i].location.coordinate.latitude
        longitude = response.businesses[i].location.coordinate.longitude
        if latitude and longitude:
            coord.append((latitude, longitude))

    return coord


# Write JSON object to a file
def write_resp_to_csv(resp_obj, file_name):
    import csv
    dicts_to_output = [
        {
            'name': biz.name,
            'id': biz.id,
            'top_category': biz.categories[0].alias if biz.categories else '',
            'rating': biz.rating,
            'review_count': biz.review_count
        }
        for biz in resp_obj.businesses
        ]
    csv_keys = ['name', 'id', 'top_category', 'rating', 'review_count']
    with open(file_name, 'w') as output_file:
        dict_writer = csv.DictWriter(output_file, csv_keys, quoting=csv.QUOTE_NONNUMERIC)
        dict_writer.writeheader()
        dict_writer.writerows(dicts_to_output)


# # Example use
# secret = 'yelp_secret.json'
# # Example yelp_secret.json
# # {
# #     "consumer_key": "my_key_here",
# #     "consumer_secret": "my_secret_here",
# #     "token": "my_token_here",
# #     "token_secret": "my_token_secret_here"
# # }
# #
# gps = {'sw_latitude': 37.900000,
#        'sw_longitude': -122.500000,
#        'ne_latitude': 37.788022,
#        'ne_longitude': -122.399797}
#
# params = {
#     'term': 'food',
#     'lang': 'en'
# }
# #
# resp = search_yelp(secret, gps, params)
# get_response_coords(resp)
# write_resp_to_csv(resp, 'businesses.csv')

# Parse response documentation:
# https://www.yelp.com/developers/documentation/v2/search_api
# resp.businesses[0].name
