# -*- coding: utf-8 -*-
"""
Created on Fri Aug 12 14:10:16 2016

@author: venkatesh
"""

from __future__ import print_function

import requests
import urllib
import pandas as pd
from bs4 import BeautifulSoup


url = "https://www.yelp.com/search?find_desc=Restaurants&find_near=times-square-new-york-2"
r = requests.get(url)
soup = BeautifulSoup(r.content)
#print(soup.prettify())
names = []
rating = []
reviews = []
price = []
address = []
links = soup.find_all("a", {"class":"biz-name"})
#name = item.find("a", {"class":"biz-name"}).contents[0]
for link in links:
    text = ''
    text = link.contents[0]
    names.append(text)
#     names.append(link.text)
#     print(link.text)
     
     
links1 = soup.find_all("span",{"class":"review-count"}) # rating-qualifier

for link in links1:
    reviews.append(link.text)
    print(link.text)
     
     

links2 = soup.find_all("span",{"class":"business-attribute price-range"})

for link in links2:
    price.append(link.text)
    print(link.text)
     
links3 = soup.find_all("address")

for link in links3:
    address.append(link.text)
    print(link.text)
i = 0     
while (i < 20):
    try:
        i += 1
        ttag = soup.find_all('a', {'class':'u-decoration-none next pagination-links_anchor'})
    
        next_url = 'https://www.yelp.com/'+ttag[0].get('href')
        
        #soup = BeautifulSoup(requests.get(next_url))
        q = requests.get(next_url)
        soup = BeautifulSoup(q.content)        
        
        
        links = soup.find_all("h3",{"class":"search-result-title"})
        
        for link in links:
            text = ''
            text = link.contents[0]
            names.append(text)
            print(link.text)
        #rating =item.find("img", {"class":"offscreen"})
        
        links1 = soup.find_all("span",{"class":"review-count rating-qualifier"})

        for link in links1:
            reviews.append(link.text)
            print(link.text)
     
     

        links2 = soup.find_all("span",{"class":"business-attribute price-range"})

        for link in links2:
            price.append(link.text)
            print(link.text)
     
        links3 = soup.find_all("address")

        for link in links3:
            address.append(link.text)            
            print(link.text)
        
        links4 = item.find("i", {"class":"star-img stars_4"})
        
        for link in links4:
            
            rating.append(link.text)
            print(link.text)
    except:
        break
        #raise
review = map(lambda x: x.strip().encode('ascii'), reviews)
price = map(lambda x: x.strip().encode('ascii'), price)
address = map(lambda x: x.strip().encode('ascii'), address)
#['rating'] = float(data[i]['rating'])
#name = map(lambda x: x.strip().encode('ascii', errors="ignore"), names)
#for i in range(0,len(rating)):
#    rating = float((rating)[i])

    

df_l = pd.DataFrame(review)
df_a = pd.DataFrame(price)
df = pd.concat([df_l, df_a], axis=1)
df_b = pd.DataFrame(address)
df = pd.concat([df, df_b], axis=1)
df_n = pd.DataFrame(names)
#data = pd.concat([df1, df_n], axis=1)


data.to_csv('C:/Users/venkatesh/Documents/s.csv', sep='\t', encoding='utf-8')

#s = df.to_csv(data, sep='\t')
#s = df.to_csv(data, sep='\t', encoding='utf-8')


#log = open("goat.txt", "w")
#for n in name:
#    print(n, file = log)