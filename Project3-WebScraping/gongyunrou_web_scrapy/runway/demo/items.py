# -*- coding: utf-8 -*-

from scrapy import Item, Field


class DemoItem(Item):
    designer= Field()
    name = Field()
    #size = Field()
    rentalPrice_4 = Field()
    rentalPrice_8 = Field()
    retailPrice =Field()
    numOfReviews =Field()
    avgRating = Field()
    fit_large = Field()
    true_to_size = Field()
    fit_small = Field()
    img = Field()
    #reviewDetails = Field()
