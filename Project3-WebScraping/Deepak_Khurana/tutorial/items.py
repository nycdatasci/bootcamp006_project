import scrapy

class DmozItem(scrapy.Item):
    title   = scrapy.Field()
    target  = scrapy.Field()
    current = scrapy.Field()
    people  = scrapy.Field()
    date    = scrapy.Field()
    #name    = scrapy.Field()
    location= scrapy.Field()
    shares  = scrapy.Field()
    #story   = scrapy.Field()
    category= scrapy.Field()
