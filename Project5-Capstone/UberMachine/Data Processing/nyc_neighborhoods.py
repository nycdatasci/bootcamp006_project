import requests
import pandas as pd

april = pd.read_csv('/Users/cholmes/Uber/april.csv')

neighborhoods = {}

#because no one can spell
fixed_neighborhoods = {'Union Port': 'Unionport', 'Richmondtown': 'Richmond', 'Woodlawn-Nordwood': 'Woodlawn', 'Ettingville' : 'Eltingville', 'Tottensville': 'Tottenville'}




for neighborhood in april.Neighborhood.unique():
    if neighborhood in fixed_neighborhoods:
        neighborhood = fixed_neighborhoods[neighborhood]

    latlong = requests.get('https://maps.googleapis.com/maps/api/geocode/json?address=' + neighborhood + 'NY' + '&key=AIzaSyBOkcsB4FSHYoz6CorB32hOv9qUOWPfcvw')
    print neighborhood, latlong.json()['results']
    lat = latlong.json()['results'][0]['geometry']['location']['lat']
    long = latlong.json()['results'][0]['geometry']['location']['lng']
    neighborhoods[neighborhood] = [lat, long]

print neighborhoods


#This file will translate our neighborhoods into longitude/latitude
