
# coding: utf-8

# In[412]:

import pandas as pd
import simplejson, urllib
import time
import neighborhoodize
hood_map = neighborhoodize.NeighborhoodMap(neighborhoodize.zillow.NEW_YORK)


# In[251]:

def print_result_page(result):
    print(len(result['results']))
    name = []
    lat = [] 
    lng = []
    for i in range(len(result['results'])):
        name.append(result['results'][i]['name'])
        lat.append(result['results'][i]['geometry']['location']['lat'])
        lng.append(result['results'][i]['geometry']['location']['lng'])     
    df = pd.DataFrame([name,lat,lng]).transpose()
    df.columns = ['Name','Lat','Lon'] 
    
    return df
#         result['results'][i]['types'])
#         print((result['results'][i]['geometry']['location']['lat']-float(coord.split(',')[0]))**2+
#               (result['results'][i]['geometry']['location']['lng']-float(coord.split(',')[1]))**2)
# if len(result['results']) = 20:
        


# In[711]:

def get_places(coord, radius, place_type):
    result = pd.DataFrame([])

    place_key = 'AIzaSyBBvY52jc279gJypRKPL0OjqUvfWRfI-NE'
    place_key_2 = 'AIzaSyBKBLYH91ChpFHWQP1--1OJaiAELy98PEY'

    url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={0}&radius={1}&types={2}&key={3}'    .format(str(coord), str(radius), str(place_type), str(place_key))
    result_first= simplejson.load(urllib.urlopen(url))

    if 'results' in result_first:
        result_0 = print_result_page(result_first)
        result = result.append(result_0)

    if len(result) == 20:
        if 'next_page_token' in result_first:
            more_url_1 = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?pagetoken={0}&key={1}'            .format(str(result_first['next_page_token']), str(place_key_2))
            time.sleep(2)
            more_result_1 = simplejson.load(urllib.urlopen(more_url_1))
            result_1 = print_result_page(more_result_1)
            result = result.append(result_1)
        
    if len(result) == 40:
        if 'next_page_token' in more_result_1:
            more_url_2 = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?pagetoken={0}&key={1}'        .format(str(more_result_1['next_page_token']), str(place_key)) 
            time.sleep(2)
            more_result_2 = simplejson.load(urllib.urlopen(more_url_2))
            result_2 = print_result_page(more_result_2)
            result = result.append(result_2)

    return result.reset_index(drop=True)


# In[756]:

# coord = '40.7711329, -73.97418739999999'
radius = '2000'
place_type = 'shopping_mall'

place_list = pd.DataFrame([])
for neighbor in neighbor_list:
    
    place = get_places(neighbor, radius, place_type)
    place_list = place_list.append(place)
    
place_list = place_list.drop_duplicates().reset_index()
place_list = get_neighbor(place_list)

place_list = place_list.reset_index().groupby([0]).count().reset_index()
place_list.columns=['Neighborhood', 'Amount']



# In[816]:

nei[nei['Neighborhood'] == 'Sunny Side']


# In[757]:

from sklearn.cluster import KMeans
kmeans = KMeans()


# In[778]:

cluster = KMeans(n_clusters = 4, n_jobs=4)
cluster.fit(nei.drop(['Neighborhood'], 1))
centers2 = cluster.cluster_centers_
labels2 = cluster.labels_


# In[410]:

def get_neighbor(df):

    neighbor_list = []
    for i in range(df.shape[0]):
        neighbor = hood_map.get_neighborhoods(df['Lat'][i],df['Lon'][i])
        neighbor_list.append(neighbor)
    
    neighbors = pd.DataFrame(neighbor_list)
    
    return neighbors


# In[576]:

neighbor_list = []
for neighbor in all_neighborhoods.values():
    neighbor = '{0},{1}'.format(neighbor[0],neighbor[1])
    neighbor_list.append(neighbor)


# In[530]:

pd.DataFrame(all_neighborhoods.values()).map(lambda x:split(',')).map(lambda x:','.join(x))

