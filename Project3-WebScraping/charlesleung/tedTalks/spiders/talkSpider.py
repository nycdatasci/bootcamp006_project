# -*- coding: utf-8 -*-
import scrapy

from tedTalks.items import TedtalksItem

class TalkSpider(scrapy.Spider):
    name = "talkSpider"
    allowed_domains = ["www.ted.com"]
    start_urls = ["http://www.ted.com/talks?page=%d" % n for n in range(1, 64)]

    def parse(self, response):
       
       divno = 0
       for sel in response.xpath('//div/div[2]'):
           item = TedtalksItem()
           titlelist = sel.xpath('h4[2]/a/text()').extract()
           namelist = sel.xpath('h4[1]/text()').extract()
           monthlist = sel.xpath('div/span[1]/span/text()').extract()
           ratedlist = sel.xpath('div/span[2]/span/text()').extract()
           linklist = sel.xpath('h4[2]/a/@href').extract()
           durlist = response.xpath('//span[@class="thumb__duration"]/text()').extract()
                      
           for i, link in enumerate(linklist):
               item['title'] = titlelist[i]
               item['name'] = namelist[i]
               item['month'] = monthlist[i]
               item['rated'] = ratedlist[i]
               item['duration'] = durlist[divno]
               item['link'] = link
               talkPage = scrapy.Request('http://www.ted.com' + link, callback = self.parse_talk)
               talkPage.meta['item'] = item
               divno += 1
               
               yield talkPage

    def parse_talk(self, response):
        item = response.meta['item']
        item['views'] = response.xpath('//span[@class="talk-sharing__value"]/text()').extract()
        item['biolink'] = response.xpath('//a[@class="l3 talk-speaker__link"]/@href')[0].extract() 
        scriptPage = scrapy.Request('http://www.ted.com' + item['link'] + '/transcript?language=en', callback = self.parse_script)
        scriptPage.meta['item'] = item
        yield scriptPage
    
    def parse_script(self, response):
        item = response.meta['item']
        phraselist = response.xpath('//span[@class="talk-transcript__fragment"]/text()').extract()
        item['script'] = ' '.join(phraselist)
        
        #find biopage
        bioPage = scrapy.Request('http://www.ted.com' + item['biolink'], callback = self.parse_bio)
        bioPage.meta['item'] = item
        yield bioPage
    
    def parse_bio(self, response):
        item = response.meta['item']
        item['dist'] = response.xpath('//div[@class="p2 profile-header__summary"]/text()').extract()
        biopar = response.xpath('//div[@class="section section--minor"]/p').extract()[0]
        item['sex'] = 'F' if((biopar.find(' She ') > -1) or (biopar.find(' she ') > -1)) else 'M'
        yield item
#get link for each triples, you want to partially fill an item instance on that page

#Name
# //*[@id="browse-results"]/div[1]/div[1]/div/div/div/div[2]/h4[1]

#Title
# //*[@id="browse-results"]/div[1]/div[1]/div/div/div/div[2]/h4[2]/a

#Month
# //*[@id="browse-results"]/div[1]/div[1]/div/div/div/div[2]/div/span[1]/span

#Rated
# //*[@id="browse-results"]/div[1]/div[1]/div/div/div/div[2]/div/span[2]/span

#Link
# //*[@id="browse-results"]/div[1]/div[1]/div/div/div/div[2]/h4[2]/a

#bioLink
# //*[@id="shoji"]/div[2]/div/div[2]/div/div/div[2]/div[1]/div/div/div/div[2]/div/div[2]/a

#transcript
#within /transcript?language=en
#http://www.ted.com/talks/elizabeth_lev_the_unheard_story_of_the_sistine_chapel/transcript?language=en

#shoji > div.shoji__door > div > div.main.talks-main > div > div > div.Grid > div.Grid__cell.w\3a 8of12\40 lg > div > div > div > div.Grid__cell.p-l\3a 1.p-l\3a 0\40 lg.w\3a 7of10.w\3a full\40 lg