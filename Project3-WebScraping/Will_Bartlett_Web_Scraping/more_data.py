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
activity = {}
for link in links:
    driver.get(link)
    soup = BeautifulSoup(driver.page_source, "lxml")
    name = soup.find('h1', {'class': 'js-startup_name u-fontSize28 u-fontWeight600 u-colorGray3 s-vgBottom0_5'}).getText().strip()
    print name

    driver.find_element_by_link_text('Activity').click()
    time.sleep(2)
    
    driver.find_element_by_link_text('People').click()
    time.sleep(2)
    soup = BeautifulSoup(driver.page_source, "lxml")
    entries_people = soup.find_all("div", {'class': 'active  dssh0 startups-show-helpers fue43 update _a _jm'})
    people_dict = {}
    for e in entries_people:
        print e.prettify()
        try:
            investor = e.find('div', {'class': 'text'})
            investor = str(investor.find('a', {'class': 'profile-link'}).getText())
        except AttributeError:
            investor = e.find('div', {'class': 'text'})
            investor = str(investor.find('a', {'class': 'startup-link'}).getText())
            print 2
        date = str(e.find('div', {'class': 'timestamp'}).getText())
        print [investor, date]
        people_dict[investor] = date
        print "len(people_dict) is"
        print len(people_dict)

    driver.find_element_by_link_text('Press').click()
    time.sleep(2)
    soup = BeautifulSoup(driver.page_source, "lxml")
    entries_press = soup.find_all("div", {'data-tab': 'press'})
    press_dict = {}
    for p in entries_press:
        print p.prettify()
        writer = str(p.find('div', {'class': 'type_and_actions'}).getText())
        date = str(p.find('div', {'class': 'timestamp'}).getText())
        headline = str(p.find('div', {'class': 'headline'}).getText())
        print [writer, date, headline]
        press_dict[writer] = [date, headline]
        print "len(press_dict) is"
        print len(people_dict)
    activity[name] = [people_dict, press_dict]
    print "LENGTH OF ACTIVITY IS"
    print len(activity)


