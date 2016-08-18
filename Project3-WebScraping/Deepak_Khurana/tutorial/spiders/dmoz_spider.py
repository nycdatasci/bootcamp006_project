import scrapy
from tutorial.items import DmozItem

class DmozSpider(scrapy.Spider):
    name = "dmoz"
    
    start_urls = [
                  #"https://www.gofundme.com/Competing-in-Rio/",
                  "https://www.gofundme.com/Medical-Illness-Healing/",
                  "https://www.gofundme.com/Volunteer-Service/",
                  "https://www.gofundme.com/Education-Schools-Learning/",
                  "https://www.gofundme.com/Accidents-Personal-Crisis/",
                  "https://www.gofundme.com/Funerals-Memorials-Tributes/",
                  "https://www.gofundme.com/Sports-Teams-Clubs/",
                  "https://www.gofundme.com/Animals-Pets/",
                  "https://www.gofundme.com/Non-Profits-Charities/",
                  #"https://www.gofundme.com/Weddings-Honeymoons/"
                  ]
    
    
    npages = 10
    for i in range(npages):
        #start_urls.append("https://www.gofundme.com/Competing-in-Rio?page="+str(i+2)+"")
        start_urls.append("https://www.gofundme.com/Medical-Illness-Healing?page="+str(i+2)+"")
        start_urls.append("https://www.gofundme.com/Volunteer-Service?page="+str(i+2)+"")
        start_urls.append("https://www.gofundme.com/Education-Schools-Learning?page="+str(i+2)+"")
        start_urls.append("https://www.gofundme.com/Accidents-Personal-Crisis?page="+str(i+2)+"")
        start_urls.append("https://www.gofundme.com/Funerals-Memorials-Tributes?page="+str(i+2)+"")
        start_urls.append("https://www.gofundme.com/Sports-Teams-Clubs?page="+str(i+2)+"")
        start_urls.append("https://www.gofundme.com/Animals-Pets?page="+str(i+2)+"")
        start_urls.append("https://www.gofundme.com/Non-Profits-Charities?page="+str(i+2)+"")
        #start_urls.append("https://www.gofundme.com/Weddings-Honeymoons?page="+str(i+2)+"")
    
    def parse(self, response):
        for href in response.xpath('//html/body/div[5]/div[2]/div/a/@href')[1:]:
            url = response.urljoin(href.extract())
            yield scrapy.Request(url, callback=self.parse_dir_contents)



    def parse_dir_contents(self, response):
        item = DmozItem()
        item['title']       = response.xpath('//html/body/div[2]/div[1]/h1/text()').extract()
        item['target']      = response.xpath('//html/body/div[2]/div[2]/div[2]/h2/span/text()').extract()
        item['current']     = response.xpath('//html/body/div[2]/div[1]/div[4]/h2/strong/text()').extract()
        item['people']      = response.xpath('//html/body/div[2]/div[1]/div[4]/div[2]/span[1]/text()').extract()
        item['date']        = response.xpath('//html/body/div[2]/div[2]/div[3]/text()').extract()
        #item['name']        = response.xpath('///*[@id="story"]/div[1]/div[2]/text()').extract()
        item['location']    = response.xpath('//html/body/div[2]/div[2]/div[5]/div[2]/a[2]/text()').extract()
        item['shares']      = response.xpath('//html/body/div[2]/div[1]/div[5]/div[1]/div[3]/div/strong/text()').extract()
        #item['story']       = response.xpath('///*[@id="story"]/div[3]/text()').extract()
        item['category']    = response.xpath('//html/body/div[2]/div[2]/div[5]/div[2]/a[1]/span/text()').extract()
        print item
        yield item


