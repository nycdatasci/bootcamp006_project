
# coding: utf-8

# In[1]:

import pandas as pd
import numpy as np
import zipfile
import requests
import StringIO
import os
from datetime import datetime
import neighborhoodize
hood_map = neighborhoodize.NeighborhoodMap(neighborhoodize.zillow.NEW_YORK)

def get_neighbor(df):

    neighbor_list = []
    for i in range(df.shape[0]):
        neighbor = hood_map.get_neighborhoods(df['Lat'][i],df['Lon'][i])
        neighbor_list.append(neighbor)
    
    neighbors = pd.DataFrame(neighbor_list)
    
    return neighbors

def get_time_piece(df):
    
    date_time = pd.DataFrame(df['Date/Time'].map(lambda x:x.split(' ')).tolist(),columns = ['Date', 'Time'])
    date_time.Time = date_time.Time.map(lambda x:x.split(':')).map(lambda x:(x[0],x[1][0])).map(lambda x:''.join(x))

    date_time['Time'].replace(to_replace= sorted(date_time['Time'].unique())                              , value = range(1,len(date_time['Time'].unique())+1), inplace= True)

    return date_time

def get_full_table(df):
    
    time = pd.DataFrame([[0]*144,range(1,145)]).transpose()
    date = pd.DataFrame([[0]*len(df['Date'].unique()),df['Date'].unique().tolist()]).transpose()
    neighbor = pd.DataFrame([[0]*len(df['Neighborhood'].unique()),df['Neighborhood'].unique().tolist()]).transpose()
    
    full_table = pd.merge(neighbor, pd.merge(date, time, how='left', on=[0]), how='left', on=[0]).drop([0, '0_x'], 1)
    full_table.columns = ['Neighborhood', 'Date', 'Time']
    
    return full_table


# In[ ]:

# geo_neighbor_table = pd.io.parsers.read_csv('D:\\gn_table.csv').drop(['Unnamed: 0'], 1)

# geo_neighbor_table.Lat = geo_neighbor_table.Lat.map(lambda x:round(x, 4))
# geo_neighbor_table.Lon = geo_neighbor_table.Lon.map(lambda x:round(x, 4))
# geo_neighbor_table.drop_duplicates(inplace = True)
# geo_neighbor_table.reset_index(drop = True)

# gn_table_geo = geo_neighbor_table[['Lat','Lon']].drop_duplicates()
# geo_neighbor_table = pd.merge(gn_table_geo.reset_index(), geo_neighbor_table.reset_index()[['Neighborhood','index']], how='left', on=['index'])\
#         .drop(['index'], 1)


# In[2]:

weather = pd.io.parsers.read_csv('D:\\weather.csv')
geo_neighbor_table = pd.io.parsers.read_csv('D:\\geo_neighbor_table.csv')


# In[57]:

taxi = pd.read_csv('D:\\yellow_tripdata_2014-07.csv')
uber = pd.io.parsers.read_csv('D:\\uber-raw-data-jul14.csv')


# In[59]:

#  taxi order table

taxi = pd.concat([taxi[' pickup_latitude'], taxi[' pickup_longitude'],taxi[' pickup_datetime'], ], axis = 1)
taxi.columns = ['Lat','Lon','Date/Time']

taxi['Lat'] = taxi['Lat'].replace(0, np.NaN)
taxi.dropna(axis=0, inplace = True)
taxi.reset_index(drop=True, inplace=True)

taxi.Lat = taxi.Lat.map(lambda x:round(x, 4))
taxi.Lon = taxi.Lon.map(lambda x:round(x, 4))

taxi = pd.merge(taxi, geo_neighbor_table, how='left', on=['Lat','Lon'])

df_to_get = taxi[taxi.isnull().any(axis=1)].reset_index(drop = True)
df_in_table = taxi.dropna(axis=0).reset_index(drop = True)

neighbors = get_neighbor(df_to_get)
neighbors.rename(columns={0: 'Neighborhood'}, inplace = True)
neighbors.drop([1], 1, inplace= True)

neighbored_df = pd.concat([df_to_get.reset_index(drop = True).drop(['Neighborhood'], 1), neighbors], axis = 1)

neighbored_df = neighbored_df.dropna(axis=0)
taxi = pd.concat([df_in_table, neighbored_df], axis = 0)

new_gn_table = neighbored_df.groupby(['Lat','Lon','Neighborhood']).count().reset_index().drop(['Date/Time'], 1)
geo_neighbor_table.append(new_gn_table)

time_piece = get_time_piece(taxi)
taxi = pd.concat([taxi.reset_index(drop = True), time_piece], axis = 1).drop(['Date/Time'], 1)
taxi = taxi.drop(['Lat'], 1).groupby(['Neighborhood','Date','Time']).count().reset_index().rename(columns={'Lon': 'Taxi_Order'})

full_table = get_full_table(taxi)

taxi =  pd.merge(full_table, taxi, how='left', on=['Neighborhood','Date','Time'])
taxi.fillna(0, inplace=True)
taxi['Taxi_Order'] = taxi['Taxi_Order'].map(lambda x:int(x))


# In[60]:

# uber order table

uber.drop(['Base'], 1, inplace=True)
uber = pd.merge(uber, geo_neighbor_table, how='left', on=['Lat','Lon'])

df_to_get = uber[uber.isnull().any(axis=1)].reset_index(drop = True)
df_in_table = uber.dropna(axis=0).reset_index(drop = True)

neighbors = get_neighbor(df_to_get)
neighbors.rename(columns={0: 'Neighborhood'}, inplace = True)
neighbors.drop([1], 1, inplace= True)

neighbored_df = pd.concat([df_to_get.reset_index(drop = True).drop(['Neighborhood'], 1), neighbors], axis = 1)
neighbored_df = neighbored_df.dropna(axis=0)
uber = pd.concat([df_in_table, neighbored_df], axis = 0)

new_gn_table = neighbored_df.groupby(['Lat','Lon','Neighborhood']).count().reset_index().drop(['Date/Time'], 1)
geo_neighbor_table.append(new_gn_table)

time_piece = get_time_piece(uber)
uber = pd.concat([uber.reset_index(drop = True), time_piece], axis = 1).drop(['Date/Time'], 1)
uber = uber.drop(['Lat'], 1).groupby(['Neighborhood','Date','Time']).count().reset_index().rename(columns={'Lon': 'Uber_Order'})

full_table = get_full_table(uber)

uber =  pd.merge(full_table, uber, how='left', on=['Neighborhood','Date','Time'])
uber.fillna(0, inplace=True)
uber['Uber_Order'] = uber['Uber_Order'].map(lambda x:int(x))


# In[61]:

whole_data = pd.merge(taxi, uber, how='left', on=['Neighborhood','Date','Time']).fillna(0)

whole_data['Uber_Order'] = whole_data['Uber_Order'].map(lambda x:int(x))
whole_data['Total_Order'] = whole_data['Taxi_Order'] + whole_data['Uber_Order']

whole_data = pd.merge(whole_data, weather.drop(['Unnamed: 0'], 1), how='left', on=['Date'])

whole_data['Weekday'] = whole_data['Date'].map(lambda x:pd.to_datetime(x)).map(lambda x:x.weekday())
whole_data['Weekend'] = whole_data['Weekday'].map(lambda x: '0' if int(x)<6 else '1')


# In[68]:

whole_data.iloc[whole_data.loc[whole_data.Date == '2014-07-04'].index]


# In[69]:

whole_data.to_csv('D:\\july.csv')


# In[85]:

april =  pd.io.parsers.read_csv('D:\\april.csv').drop(['Unnamed: 0'], 1)
may =  pd.io.parsers.read_csv('D:\\may.csv').drop(['Unnamed: 0'], 1)
june =  pd.io.parsers.read_csv('D:\\june.csv').drop(['Unnamed: 0'], 1)
july =  pd.io.parsers.read_csv('D:\\july.csv').drop(['Unnamed: 0'], 1)
august =  pd.io.parsers.read_csv('D:\\august.csv').drop(['Unnamed: 0'], 1)
september =  pd.io.parsers.read_csv('D:\\september.csv').drop(['Unnamed: 0'], 1)


# In[110]:

ubermachine = pd.concat([april, may, june, july, august, september], axis =0)

