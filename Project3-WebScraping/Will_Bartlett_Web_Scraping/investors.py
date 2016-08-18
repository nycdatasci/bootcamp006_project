from selenium import webdriver
from bs4 import BeautifulSoup
import time

#get links
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
links_lst = soup.find_all('div', {'class': 'name'})
links = map(lambda link: str(link.find('a').get('href')), links_lst)
activity = {}
skipped = 0
print len(links)

# for each link
for link in links:

    print '___________________NEW COMPANY_______________'
    try:
        #get to "people" on angelist page
        driver.get(link)
        soup = BeautifulSoup(driver.page_source, "lxml")
        name = soup.find('h1', {
            'class': 'js-startup_name u-fontSize28 u-fontWeight600 u-colorGray3 s-vgBottom0_5'}).getText().strip()
        print name
        driver.find_element_by_link_text('Activity').click()
        time.sleep(2)
        driver.find_element_by_link_text('People').click()
        time.sleep(2)

        ##extend "people" activity page
        more = driver.find_element_by_xpath('//*[@id="root"]/div[4]/div/div/div[2]/div/div[2]/div[1]/div/div[2]/div/div[2]/a')
        timeout = 15
        while True:
            start = time.time()
            soup = BeautifulSoup(driver.page_source, "lxml")
            if soup.find('a', {'class': 'g-feed_more more disabled'}):
                print "NO MORE FOUND"
                break
            try:
                more.click()
                if time.time() - start > timeout:
                    break
            except:
                try:
                    time.sleep(6)
                    more.click()
                    if time.time() - start > timeout:
                        break
                except:
                    break

        # get each activity entry
        soup = BeautifulSoup(driver.page_source, "lxml")
        entries_people = soup.find_all("div", {'data-_tn': 'startups/show/helpers/update'})
        print ''.join(["# of activity entries for ", name])
        print len(entries_people)

        #create activity log
        people_dict = {}
        for e in entries_people:
            try:
                investor = e.find('div', {'class': 'text'})
                investor = investor.find('a', {'data-type': 'User'}).getText()
            except AttributeError:
                try:
                    investor = e.find('div', {'class': 'text'})
                    investor = investor.find('a', {'data-type': 'Startup'}).getText()
                    print "STARTUP USED"
                except AttributeError:
                    print "PASSSSSSSSSSSSSED"
                    investor = "INVESTOR PASSED"
            try:
                date = e.find('div', {'class': 'timestamp'}).getText()
            except:
                date = "DATE PASSED"
            people_dict[investor] = date
            print ', '.join([investor, date, str(len(people_dict))])
        print " ".join([name, "investor length is", str(len(people_dict))])
        activity[name] = [people_dict]
        print "_______________________________________"
        print "".join(["Companies recorded:", str(len(activity))])
    except:
        print ' '.join([link, "SKIPPED"])
        skipped = skipped + 1
        print ''.join(["Total Skipped: ", skipped])

import pandas as pd
pd.set_option('expand_frame_repr', False)
activity_df = pd.DataFrame(activity).transpose()
activity_df.to_excel('activity.xlsx')
print activity_df