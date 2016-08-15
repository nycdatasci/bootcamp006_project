from scrapy import Spider
from scrapy.selector import Selector
from demo.items import DemoItem
from operator import methodcaller
from selenium import webdriver
from scrapy.http import TextResponse
from selenium.common.exceptions import TimeoutException
import scrapy 
import json
import re
import time


class DemoSpider(Spider):
	name = 'runway'
	allowed_urls = ['https://www.renttherunway.com']
	start_urls = ['https://www.renttherunway.com/products/dress']

	def __init__(self):
		self.driver = webdriver.Chrome("/Users/crystal/Desktop/gongyunrou_web_scrapy/runway/chromedriver")
 

	def parse(self, response):
		page_base_urls = 'https://www.renttherunway.com/products/dress'		
		last_page_num = DemoSpider.get_last_page_num(response)
		page_urls = [page_base_urls+ "?page=" + str(pageNumber) for pageNumber in range(1, DemoSpider.get_last_page_num(response)+1)]
		item_urls = []
		for url in page_urls:
			time.sleep(1)
			self.driver.get(url)
			response = TextResponse(url = self.driver.current_url, body = self.driver.page_source, encoding = "utf-8")
			item_urls.extend(self.parse_page(response))
		i = 0
		for item_url in item_urls:

			item_base_url = 'https://www.renttherunway.com'
			entry_url = item_base_url + item_url
			try:
				self.driver.get(entry_url)
			except TimeoutException:
				continue
			#self.driver.get(entry_url)
			if i==0:
				self.driver.find_element_by_class_name('mfp-close').click()
			i += 1
			time.sleep(1)
			response = TextResponse(url = self.driver.current_url, body = self.driver.page_source, encoding = "utf-8")
			try:
				dress_item = self.parse_entry(response, self.driver)
				yield dress_item
			except:
				pass
			# yield scrapy.Request(page_url, callback=self.parse_page)
		self.driver.close()


	def parse_page(self, response):
		# item_base_url = 'https://www.renttherunway.com'
		# for i in range(1, 60):
		# 	print i 
		# 	itemXPath = '//*[@id="page-content"]/div[1]/div[2]/div/div[2]/div[3]/div[3]/div['+str(i)+']/a[1]/@href'
		# 	itemPath = response.xpath(itemXPath).extract()
		# 	entry_url = item_base_url+itemPath[0]
		# 	yield scrapy.Request(entry_url, callback=self.parse_entry)
		
		links = response.xpath('//div[@class="grid-product-card"]/a[1]/@href').extract()
		print links
		return links
		# for url in links:
		# 	entry_url = item_base_url+url

			# yield scrapy.Request(entry_url, callback=self.parse_entry)
			
			
		

	def parse_entry(self, response, driver):

		'''
	
		size = Field()
		rentalPrice = Field()
		
		'''
		# designer = response.xpath('//*[@id="page-content"]/div[1]/div[1]/div[3]/div[1]/h1/a/text()').extract()
		# designer = designer[0].strip(' \t\n\r').lower()
		designer=map(unicode.strip,response.xpath('//h1[@class="h2 product-designer"]/a/text()').extract())
		print "designer"
		print designer


		# name = response.xpath('//*[@id="page-content"]/div[1]/div[1]/div[3]/h2/text()').extract()
		# name = name[0].strip(' \t\n\r').lower()
		name= map(unicode.strip,response.xpath('//h2[@class="display-name body-copy"]/text()').extract())
		print "dressname"
		print name

		'''
		sizes = response.xpath('//*[@id="product-primary-size"]/option[4]/@data-designer-size').extract()
		print sizes
		'''


		# rentalPrice = response.xpath('//span[@class="product-price-rental-label"]/span/text()').extract()
		# rentalPrice = float(rentalPrice[0])
		rentalPrice_4 = driver.find_element_by_class_name("product-price-rental-label").text
		rentalPrice_4 = float(rentalPrice_4[1:])

		print "rentalPrice_4"
		print rentalPrice_4


		try:
			driver.find_element_by_xpath('//input[@id="rentaldays-8"]').click()
			time.sleep(1)
			rentalPrice_8 = driver.find_element_by_class_name("product-price-rental-label").text
			rentalPrice_8 = float(rentalPrice_8[1:])
			print "rentalPrice_8"
			print rentalPrice_8
		except Exception as e:
			print e
			rentalPrice_8 = ""
			print "rentalPrice_8"
			print rentalPrice_8
			pass


		# retailPrice = response.xpath('//*[@id="page-content"]/div[1]/div[1]/div[3]/div[2]/div[2]').extract()
		#retailPrice = response.xpath('//div[@class="product-price product-price-retail body-copy"]/text()').extract()
		retailPrice = map(unicode.strip, response.xpath('//div[@class="product-price product-price-retail body-copy"]/text()').extract())
		#[u'$495\n\t\t\t\t\tretail']
		retailPrice = float(retailPrice[0].split()[0][1:])
		print "retailPrice"
		print retailPrice

		# reviews = response.xpath('//*[@id="page-content"]/div[1]/div[1]/div[3]/div[3]/a/span[1]').extract()
		numOfReviews = response.xpath('//div[@class = "h2 review-count"]/text()').extract()
		
		try:
			numOfReviews = numOfReviews[0].split()[0]
			numOfReviews =  int(numOfReviews)
			print "numOfReviews"
			print numOfReviews
		except ValueError:
			numOfReviews = 0


		# reviews = reviews[0][-10]
		# print "reviews"
		# print reviews
		
		# fit = []
		# for i in range(1, 4):
			# fit.append(int(response.xpath('//*[@id="reviews-partial"]/div[1]/div[1]/div/div[2]/table/tbody/tr['+str(i)+']/td[2]/text()').extract()[0]))
			# fit.append(int(response.xpath('//div[@class="fit-summary"]/table[@class="review_overall_fit_bars review_summary_section_content"]/text.()').extract()))
		fit = response.xpath('//td[@class = "count label"]/text()').extract()
		if len(fit) == 0:
			fit = [0,0,0]
		else:
			fit  = [ int(x) for x in fit ]
		print "fit"
		print fit
		
		avgRating = response.xpath('//div[@class="reviews-average-rating review summary_section_content"]/div/@class').extract()
		try:
			avgRating = int(avgRating[0].split(u'reb-gold-stars-')[1])
			print "Average Rating" 
			# avgRating = int(avgRating[0])
			print avgRating
		except ValueError:
			avgRating = 0



		#img = response.xpath('//*[@id="page-content"]/div[1]/div[1]/div[4]/div/div[1]/div[2]/div/div[1]/a/img/@src').extract()
		img = response.xpath('//div[@class="thumb-viewport"]/div/div[1]/a/@data-original-src').extract()
		print "img"
		print img


		item = DemoItem()
		item['designer'] = designer[0].encode('utf-8').rstrip()
		item['name']= name[0]
		#item['size']=size
		item['rentalPrice_4']=rentalPrice_4
		item['rentalPrice_8']=rentalPrice_8
		item['retailPrice']=retailPrice
		item['numOfReviews']=numOfReviews
		item['avgRating']= avgRating
		item['fit_large']=fit[0]
		item['true_to_size']=fit[1]
		item['fit_small'] = fit[2]
		item['img']=img[0]
		#item['reviewDetails']=reviewDetails

		return item

		# '''
		# # all user review details as dictionary saved in this list for a single item
		# # inside the dictionary may include size, rating, age, weight, height, reviewTitle, reviewBody, etc
		# reviewDetails = []

		# # combined with reveiwDetails
		# ratings = response.xpath('//div[@class="review-content"]/div[1]/@class').extract()
		# ratings = map(methodcaller("split", "reb-gold-stars-"), ratings)
		# ratings = map(lambda x: x[1], ratings)
		# print "ratings, num: ", len(ratings)

		# # reviewer information
		# label = response.xpath('//div[@class="review-detail-label label"]/text()').extract()
		# value = response.xpath('//div[@class="review-detail-value label"]/text()').extract()
		# print "label, num: ", len(label)
		# print "value, num: ", len(value)
		# assert(len(label) == len(value))

		# # more reviewer information
		# sLabel = response.xpath('//span[@class="review-detail-label"]/text()').extract()
		# sValue = response.xpath('//strong[@class="review-detail-value"]/text()').extract()
		# print "sLabel, num: ", len(sLabel)
		# print "sValue, num: ", len(sValue)
		# assert(len(sLabel) == len(sValue))

		# # combining reviewer information
		# label += sLabel
		# value += sValue

		# # creating a dictionary for all review details
		# userDict = None
		# for i,j in zip(label, value):
		# 	if i == u"Size worn: ":
		# 		if userDict != None:
		# 			reviewDetails.append(userDict)
		# 		userDict = {}
		# 	userDict[i] = j
		# if userDict:
		# 	reviewDetails.append(userDict)

		# print "reviewDetails", len(reviewDetails)
		# assert( len(reviewDetails) == len(ratings))
		

		# # adding written review details ( title and body)
		# reviewTitle = response.xpath('//div[@class="review-title dek-one"]/text()').extract()
		# reviewBody = response.xpath('//div[@class="review-text body-copy"]/text()').extract()
		# print "reviewTitle, num: ", len(reviewTitle)
		# print "reviewBody, num: ", len(reviewBody)
		# assert(len(reviewTitle) == len(reviewBody))
		# assert(len(reviewTitle) == len(reviewDetails))

		# for d, r, t, b in zip(reviewDetails, ratings, reviewTitle, reviewBody):
		# 	d["rating"]= r
		# 	d["reviewTitle"]= t
		# 	d["reviewTitle"]= b

		# # only need to add this one to DemoItem as userReview info
		# print reviewDetails
		# '''

	@staticmethod
	def get_last_page_num(response):
		return int(response.xpath('//*[@id="page-content"]/div[1]/div[2]/div/div[2]/div[3]/nav/div/p/span[2]/text()').extract()[0].split("/")[1])






