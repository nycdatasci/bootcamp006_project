# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class TedtalksItem(scrapy.Item):
    title = scrapy.Field()
    name = scrapy.Field()
    month = scrapy.Field()
    rated = scrapy.Field()
    link = scrapy.Field()
    duration = scrapy.Field()
    views = scrapy.Field()
    script = scrapy.Field()
    biolink = scrapy.Field()
    sex = scrapy.Field()
    dist = scrapy.Field()
