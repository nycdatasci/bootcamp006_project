from bs4 import BeautifulSoup as bs
import re
import requests
import pandas as pd

url_list,mugURL,mugName, mugCity, mugCountry, mugEdition, mugOwners, mugTrader, mugSeeker = ([] for i in range(9))

for i in range(1,534):
	url_list.append('http://fredorange.com/mugs/?&pg=' + str(i))

for url in url_list:
	raw = bs(requests.get(url).text, "lxml")
	titleDiv = raw.findAll("div", { "class" : "mugTitle" })
	mugURL.append([url2.a.get('href') for url2 in titleDiv])
	
mugURL = [y for x in mugURL for y in x]

for url in mugURL:
	raw = bs(requests.get(url).text, "lxml")

	nameDiv = raw.findAll("div", { "id" : "c3" })
	detailDiv = raw.findAll("div", { "id" : "mugData" })
	dat = raw.find('div', id='mugData').find('ul').find_all('li')[-1].text

	mugName.append([tag.h1.string for tag in nameDiv])

	for tag in detailDiv:
		try: 
			mugCity.append(tag.find('ul').find(text='City:').findNext('a').text)
		except(AttributeError):
			mugCity.append("NA")

	for tag in detailDiv:
		mugCountry.append(tag.find('ul').find(text='Country:').findNext('a').text)
		mugEdition.append(tag.find('ul').find(text='Edition:').findNext('a').text)
		

	n=[re.findall(r'\d+', dat)]
	mugOwners.append(n[0][0])   
	mugTrader.append(n[0][1])   
	mugSeeker.append(n[0][2])  


mug = {
	'Name': mugName,
    'City': mugCity,
    'Country': mugCountry,
    'Edition': mugEdition,
    'Owner': mugOwners
    'Trader': mugTrader,
    'Seeker': mugSeeker
}

mugdf = pd.DataFrame(mug)

mugdf.to_csv('mug.csv', sep='\t', encoding='utf-8')