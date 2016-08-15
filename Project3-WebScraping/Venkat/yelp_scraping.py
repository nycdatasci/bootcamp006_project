from __future__ import print_function
import requests
import pandas as pd
from bs4 import BeautifulSoup

url = "https://www.yelp.com/search?find_desc=Restaurants&find_near=times-square-new-york-2"
r = requests.get(url)
soup = BeautifulSoup(r.content)
#print(soup.prettify())
names = []
reviews = []
price = []
address = []
links = soup.find_all("h3",{"class":"search-result-title"})

for link in links:
    text = ''
    for string in link.strings:
        if string.strip():
            text = text + string.strip()
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
            
            names.append(link.text)
            print(link.text)
     
        
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
    
    except:
        break
        #raise
review = map(lambda x: x.strip().encode('ascii'), reviews)
price = map(lambda x: x.strip().encode('ascii'), price)
address = map(lambda x: x.strip().encode('ascii'), address)
name = map(lambda x: x.strip().encode('ascii', errors="ignore"), names)


df_l = pd.DataFrame(review)
df_a = pd.DataFrame(price)
df = pd.concat([df_l, df_a], axis=1)
df_b = pd.DataFrame(address)
df1 = pd.concat([df, df_b], axis=1)
df_n = pd.DataFrame(names)
data = pd.concat([df1, df_n], axis=1)


df1.to_csv('C:/Users/venkatesh/Documents/s.csv', sep='\t', encoding='utf-8')

#s = df.to_csv(data, sep='\t')
#s = df.to_csv(data, sep='\t', encoding='utf-8')
import codecs
writer = codecs.open("ss.csv","w", encoding = "utf-8")
for x in range(0, 230):
    writer.write(review[x])
    writer.write(",")
    writer.write(price[x])
    writer.write(",")
    writer.write(address[x])
    writer.write("\n")
writer.close()
    



log = open("goat.txt", "w")
for n in name:
    print(n, file = log)