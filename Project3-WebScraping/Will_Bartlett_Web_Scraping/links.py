from selenium import webdriver
from bs4 import BeautifulSoup
import time
driver = webdriver.Chrome()
driver.get("https://angel.co/health-care")
time.sleep(6)
timeout = 55
start = time.time()
while driver.find_elements_by_xpath('//*[@id="root"]/div[4]/div[2]/div[2]/div/div[3]'):
    driver.find_elements_by_xpath('//*[@id="root"]/div[4]/div[2]/div[2]/div/div[3]')[0].click()
    time.sleep(2)
    if time.time() - start > timeout:
        break
soup = BeautifulSoup(driver.page_source, "lxml")
links_lst = soup.find_all('div', {'class':'name'})
links = map(lambda link:str(link.find('a').get('href')), links_lst)
info = {}


for link in links:


    driver.get(link)
    time.sleep(3)
    soup = BeautifulSoup(driver.page_source, "lxml")
    name = soup.find('h1', {'class': 'js-startup_name u-fontSize28 u-fontWeight600 u-colorGray3 s-vgBottom0_5'}).getText().strip()
    try:
        blurb = soup.find('p').getText().strip()
    except AttributeError:
        blurb = 'no blurb'
        print 2
    location = soup.find('span', {'class': 'js-location_tags'}).getText().strip()
    market = soup.find('span', {"class": "js-market_tags"})
    market_a = market.find_all("a", {"class": "tag"})
    market_tags = map(lambda t: t.getText(), market_a)
    emp_num = soup.find('span', {'class': 'js-company_size'}).getText()
    info[name] = [blurb, location, market_tags, emp_num]
    print len(info)
    print name

import pandas as pd
info_df = pd.DataFrame(info).transpose()
info_df.to_excel('info_df.xlsx')
