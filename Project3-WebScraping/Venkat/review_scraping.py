# -*- coding: utf-8 -*-
"""
Created on Fri Aug 12 16:04:55 2016

@author: venkatesh
"""

import requests
import pandas as pd
from bs4 import BeautifulSoup

url = "https://www.yelp.com/search?find_desc=Restaurants&find_loc=Times+Square%2C+NY&ns=1"
info = pd.DataFrame()
r = requests.get(url)
info = {}
soup = BeautifulSoup(r.content)
#print(soup.prettify())
links = soup.find_all("h3",{"class":"search-result-title"})
for link in links[1:]:
    res_name = ''.join(map(lambda x: x.strip(), link.strings))
    print res_name
    info[res_name] = []
    res_url = 'https://www.yelp.com/' + link.find('a').get('href')
    res = requests.get(res_url)
    soup1 = BeautifulSoup(res.content)
    print res_url
#    next_url = 'https://www.yelp.com/'+ttag[0].get('href')
#    q = requests.get(next_url)
#    soup1 = BeautifulSoup(q.content)
    reviews = soup1.find_all("p", {"itemprop": "description"})
    for review in reviews:
#        print review.text
        info[res_name].append(review.text)
        


i = 0     
while (i < 20): 
    try:
        i += 1
        ttag = soup.find_all('a', {'class':'u-decoration-none next pagination-links_anchor'})
        
        next_url = 'https://www.yelp.com/'+ttag[0].get('href')
        #next_url = ttag[-1].get('href')
#        print next_url
 #       '/search?find_desc=Restaurants&find_loc=Times+Square%2C+NY&start=10'
        q = requests.get(next_url)
        soup = BeautifulSoup(q.content)
        links = soup.find_all("h3",{"class":"search-result-title"})
#        print len(links[1:])
        for link in links[1:]:
            res_name = ''.join(map(lambda x: x.strip(), link.strings))
            print res_name
            info[res_name] = []
            res_url = 'https://www.yelp.com/' + link.find('a').get('href')
            res = requests.get(res_url)
            soup1 = BeautifulSoup(res.content)
 #           print res_url
#    next_url = 'https://www.yelp.com/'+ttag[0].get('href')
#    q = requests.get(next_url)
#    soup = BeautifulSoup(q.content)
            reviews = soup1.find_all("p", {"itemprop": "description"})
            for review in reviews:
#                print review.text
                info[res_name].append(review.text)

    except:
        break
    
    
print info

x = pd.DataFrame(info.items(), columns=['Restaurant', 'Review']) 

#x.to_csv('C:/Users/venkatesh/Documents/review.csv', sep='\t', encoding='utf-8')


import codecs
writer = codecs.open("daa.csv","w", encoding = "utf-8")
for key, val in info.items():
    writer.write(key)
    writer.write(",")
    value = str(val).replace(",", "")
    writer.write(value)
    writer.write("\n")
writer.close()


