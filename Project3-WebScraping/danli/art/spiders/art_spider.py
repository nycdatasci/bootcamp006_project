# -*- coding: utf-8 -*-
import scrapy
import json
from scrapy import Spider, Request
from scrapy.selector import Selector
from art.items import ArtItem

class ArtSpider(Spider):
    name = 'art'
    allowed_urls = ['artcyclopedia.com']

    f = open("artist_urls.txt", 'r') 
    # There are 9062 artist urls
    start_urls = [url.strip() for url in f.readlines()]
    f.close()
    
    def parse(self, response):
        item = ArtItem()

        item['artist'] = response.xpath('//h1[@style="margin-bottom:5px; font-size:30px;"]/text()').extract()

        try:
            item['details'] = response.xpath('//font[@style="font-size:18px;"]/b/text()').extract()
        except Exception as e:
            item['details'] = None

        try:
            item['genre'] = response.xpath('//font[@style="font-size:18px;"]/b/a/text()').extract()
        except Exception as e:
            item['genre'] = None       

        try:
            item['museums'] = response.xpath('//font[contains(.,"Museums and Public Art Galleries Worldwide")]/parent::b/following-sibling::blockquote[1]//a[@target="_blank"]/text()').extract()
        except Exception as e:
            item['museums'] = None

        yield item