'''
------------------------------------------------------------------------------
Creator: Bernard Ong
Created: Aug 2016
Project: Web Scraping Project
Purpose: eBook Title Words Recommender
------------------------------------------------------------------------------
'''

import re
import sys
import string
import requests
from bs4 import BeautifulSoup

reload(sys)
sys.setdefaultencoding("utf-8")

class AmazonKindle:

	AMZ_ROOT = 'https://www.amazon.com/'

	def __init__(self, query):
		# Preprocess Search Query
		exclude = set(string.punctuation)
		self.query = ''.join(ch for ch in query if ch not in exclude)
		# Get maximum page to retrieve
		self.maximum = self.maxPage()

	def buildURL(self, keyword, page):
		# Build URL from keyword and page numbers
		params = 's/rh=n%3A133140011%2Ck%3A' + str(keyword) + '&page=' + str(int(page))
		return self.AMZ_ROOT + params

	def retrieveSource(self, url):
		# Retrieve raw html as beautiful soup form
		headers = {
            'Accept' : 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
            'Accept-Encoding' : 'gzip, deflate, sdch',
            'Accept-Language' : 'en-US,en;q=0.8,ar;q=0.6',
            'Cache-Control' : 'max-age=0',
            'Connection' : 'keep-alive',
            'Cookie' : """x-wl-uid=1sKNa/Tu+dnJiObdvPzWmOWvHoQHs7SbunE2P1wtUNyXZoFmxflkD74oIUtjg6je4M1DNJgid9D73eSQ/1szSzyViGKhQNfYhzyrIF+0sYuCypgc4oFkknUPA4xZ0Aeienl0XdGyC/GE=; bhqns=d0sdm59meaki4gsmq4gsdpv5k3; x-main="HSxQRguP7RaA0XNnOlvYoSu?jhNN?Gbu"; b2b-main=0; s_vnum=1857890451251%26vn%3D3; appstore-devportal-locale=en_US; aws_lang=en; aws-target-data=%7B%22support%22%3A%221%22%7D; s_dslv=1428249404729; aws-target-visitor-id=1428249404407-552911.26_29; s_sq=%5B%5BB%5D%5D; _mkto_trk=id':'810-GRW-452&token':'_mch-amazon.com-1428249580944-19910; __utma=194891197.1337326270.1428250257.1428250257.1428250257.1; __utmc=194891197; __utmz=194891197.1428250257.1.1.utmccn=(referral)|utmcsr=google.com.eg|utmcct=/|utmcmd=referral; pN=1; s_pers=%20s_fid%3D3E166C526619A613-1D8EA49490B2AAB8%7C1491407981003%3B%20s_dl%3D1%7C1428251381006%3B%20gpv_page%3DUS%253AAS%253APADS-how-it-works%7C1428251381011%3B%20s_ev15%3D%255B%255B%2527NSGoogle%2527%252C%25271428249581020%2527%255D%255D%7C1586102381020%3B%20s_invisit%3Dtrue%7C1428252056959%3B%20s_nr%3D1428250256962-Repeat%7C1436026256962%3B; s_sess=%20c_m%3Dwww.google.com.egNatural%2520Search%3B%20s_ppvl%3DUS%25253AAS%25253APADS-how-it-works%252C56%252C56%252C955%252C1920%252C955%252C1920%252C1080%252C1%252CL%3B%20s_cc%3Dtrue%3B%20s_sq%3D%3B%20s_ppv%3DUS%25253AAS%25253APADS-how-it-works%252C91%252C56%252C1555%252C1920%252C955%252C1920%252C1080%252C1%252CL%3B; session-token=opZ2DsH3n42Pz4spSbxaz9oQx5kGTKq1pj4n5WE+pGWvRBk6IyjvUQWw/GrDH2dGqbwR7d97ibcno5K4B1yUnpoVceB8aEDziGrWpU+I+tHBkWWy0ZMorJZowa3DMS40gfMTAc1Gd/o4ZpYOibC1SPQEzU3Eg2OslDRNyys+e90lJaothbV12MO62XiHFsnjjhCVFk8Ztr3hRgnl5oC1NvbZ9lZ7QjmhDP9wXgnYmG6wevsgqdukBbKoohHv1kGOWg60CWLBFS/RhVqukMAO5g==; skin=noskin; ubid-main=182-4576256-0011665; session-id-time=2082787201l; session-id=180-4378028-6113727; csm-hit=1RPD28BQJKGVJNHSZKFR+s-1RPD28BQJKGVJNHSZKFR|1428854614288""",
            'Host' : 'www.amazon.com',
            'User-Agent' : 'Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.118 Safari/537a'
        }
		response = requests.get(url, headers=headers)
		return BeautifulSoup(response.content, 'html.parser')

	def maxPage(self):
		# Get maximum number of pages
		source = self.retrieveSource(self.buildURL(self.query, 1))
		max_page = source.find('span', {'class' : 'pagnDisabled'})
		return int(max_page.text) if max_page else 0

	def processRecord(self, html):
		# Single item processor
		title = html.find('h2').text
		price = html.find('span', {'class':'s-price'})
		rating = html.find('span', {'class':'a-icon-alt'})
		reviews = html.find('div', {'class':'a-span-last'})

		# Process Reviews
		if reviews:
			for a in reviews.find_all(lambda a: (a.name=='a' and \
									'customerReviews' in a['href']), href=True):
				stringer = a.text
				stringer = stringer.replace(',','')
				reviews = int(stringer)

		# Return Result
		if title and price and rating and reviews:
			return title, float(price.text.replace(',', '')[1:]), float(rating.text.replace(' out of 5 stars', '')), reviews
		else: return None

	def processPage(self, html):
		# Page processor
		listing = html.findAll('li', id=lambda x: x and x.startswith('result_'))
		for i in listing:
			yield self.processRecord(i)

	def __iter__(self):
		# Amazon Kindle results iterator
		for i in range(1, self.maximum+1):
			print 'Scraping and Processing: Page '+str(i)+' / '+str(self.maximum)
			raw_html = self.retrieveSource(self.buildURL(self.query, i))
			for j in self.processPage(raw_html):
				yield j

if __name__ == '__main__':
	# this is not use from main scraper, only used for testing and debugging the class
	amz = AmazonKindle('machine learning')
	for a in amz:
		print a
