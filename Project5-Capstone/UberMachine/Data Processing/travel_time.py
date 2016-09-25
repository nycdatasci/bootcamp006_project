
import pandas as pd
import simplejson, urllib
orig_coord = '40.718267,-73.893224'
dest_coord = '40.73733,-73.9307177'
key = 'AIzaSyBU2bHLWxzS3JPTIEmP4tBQ-DrG6Xeb1AA'
url = "https://maps.googleapis.com/maps/api/distancematrix/json?origins={0}&destinations={1}&mode=driving&departure_time=now&language=en-EN&key={2}".    format(str(orig_coord),str(dest_coord),str(key))
    
result= simplejson.load(urllib.urlopen(url))

driving_distanct_km = result['rows'][0]['elements'][0]['distance']['text']
driving_distanct_mile = '{0} mile'.format(round(float(driving_distanct_km.split(' ')[0])*0.621371, 1))
driving_time = result['rows'][0]['elements'][0]['duration']['text']
driving_time_traffic = result['rows'][0]['elements'][0]['duration_in_traffic']['text']

df = pd.DataFrame([driving_distanct_km,driving_distanct_mile,driving_time,driving_time_traffic]).transpose()
df.columns = ['Distance_KM','Distance_mile','Time','Time_Traffic']

df

