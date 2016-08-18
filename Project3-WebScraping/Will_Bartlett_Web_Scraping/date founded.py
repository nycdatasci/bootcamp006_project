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
founders_dict = {}
skipped_founders = 0
print len(links)

for link in links:
    driver.get(link)
    time.sleep(3)
    soup = BeautifulSoup(driver.page_source, "lxml")
    name = soup.find('h1', {
        'class': 'js-startup_name u-fontSize28 u-fontWeight600 u-colorGray3 s-vgBottom0_5'}).getText().strip()
    print name
    founders = soup.find('div', {'class': 'founders section'})
    try:
        founders_all = founders.find_all('div', {'class': 'name'})
        founder = map(lambda founder: founder.find('a').getText(), founders_all)
    except AttributeError:
        try:
            time.sleep(3)
            founders_all = founders.find_all('div', {'class': 'name'})
            founder = map(lambda founder: founder.find('a').getText(), founders_all)
        except:
            founder = "FOUNDER SKIPPED"
            skipped_founders = skipped_founders + 1
            print founder
    founders_dict[name] = [founder]
    print [name, founder]
    print len(founders_dict)
    print'_________________________________'

import pandas as pd
pd.set_option('expand_frame_repr', False)
founders_dict = pd.DataFrame(founders_dict).transpose()
founders_dict.to_excel('founders_dict.xlsx')
print founders_dict