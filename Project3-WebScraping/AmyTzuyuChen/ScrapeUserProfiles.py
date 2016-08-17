from bs4 import BeautifulSoup as bs
import re
import requests
import pandas as pd

url_list_user, userName, userCity, userCountry, userMug,userPercent,userTrading,userLooking= ([] for i in range(8))

for i in range(1,438):
	url_list_user.append('http://fredorange.com/users/?&pg=' + str(i))

for url in url_list_user:
	raw = bs(requests.get(url).text, "lxml")
	
	titleDiv = raw.findAll("div", { "class" : "mugTitle" })
	collection = raw.findAll(text=' of 5329 ')
	looking = raw.findAll(text='Looking For:')

	userName.append([tag.a.string for tag in titleDiv])

	for tag in titleDiv:
		try: 
			userCity.append(tag.findAll('a')[1].text)
		except(IndexError):
			userCity.append("NA")

	for tag in titleDiv:
		try: 
			userCountry.append(tag.findAll('a')[2].text)
		except(IndexError):
			userCountry.append("NA")

	userMug.append([tag.findPreviousSibling('a').text for tag in collection])
	userPercent.append([tag.findNextSibling('small').text for tag in collection])
	userTrading.append([tag.findNext('a').text for tag in collection])
	userLooking.append([tag.findNext('a').text for tag in looking])

userName = [y for x in userName for y in x]
userMug = [y for x in userMug for y in x]
userPercent = [y for x in userPercent for y in x]
userTrading = [y for x in userTrading for y in x]
userLooking = [y for x in userLooking for y in x]

user = {
    'userName': userName,
    'City': userCity,
    'Country': userCountry,
    'MugsOwned': userMug,
    'PercentageOwned': userPercent,
    'Trading': userTrading,
    'Looking': userLooking,
}

userdf = pd.DataFrame(user)

userdf.to_csv('user.csv', sep='\t', encoding='utf-8')