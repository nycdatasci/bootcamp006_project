from bs4 import BeautifulSoup
from compiler.ast import flatten
import os
import urllib2
import time
import numpy as np
import csv
os.chdir("/Users/binfang/Documents/NYCDSA/project/Project_3")
# Scrape returned search results
webpage = []
ls = []
miss = []
webpage = []
zipcode = 98101
def pkg_url(page):
    return "http://www.carfax.com/vehicles/Used--" + str(zipcode) + "/page-" + page

pg_num = 1
while pg_num <= 19:
    url = pkg_url(str(pg_num))
    print url
    read = urllib2.urlopen(url).read()
    soup = BeautifulSoup(read, "lxml")
    flag = ""
    match1 = soup.find_all("div", {"class": "basic-detail"})
    ls = []
    for div in match1:
        href = div.find("a", {"class": "j-singlepage"}).get("href")
        if href:
            ls += ["http://www.carfax.com" + href]
            #print ls[-1]
    flag = flag + href
    if len(flag) > 0:
        pg_num += 1
        # Scrape each web link
    str1 = "Price:"
    str2 = "Mileage:"
    str3 = "Location:"
    str4 = "Engine:"
    str5 = "Transmission:"
    str6 = "Drive Type:"
    str7 = "Fuel Type:"
    str8 = "MPG City/Hwy:"
    str9 = "Exterior Color:"
    str10 = "Interior Color:"
    str_all = [str1, str2, str3, str4, str5, str6, str7, str8, str9, str10]
    for num2 in range(len(ls)):
        info = []
        read = urllib2.urlopen(ls[num2]).read()
        soup = BeautifulSoup(read, "lxml")
        # Scrape car name
        carname = str(soup.find_all("div", {"class": "left-details left-details-abTest-"}))
        carname = carname[carname.find("n<h1>") + len("</h1>"):carname.find("</h1>")]
        carname = carname.split(" ")[0:3]
        # Scrape other information of car
        match1 = soup.find_all("div", {"class": "vehicle-information"})
        for tag in match1:
            strong = tag.find_all("strong")
            info = tag.text
            info = ' '.join(info.split())
            ls2 = []
            for n in range(0, 9):
                match_str = info[info.find(str_all[n]) + len(str_all[n]):info.find(str_all[n+1])]
                match_str = match_str.replace(",", "")
                match_str = match_str.replace("$", "")
                match_str = match_str.replace(" ", "")
                ls2.append(str(match_str))
            mpg_split = ls2[7].split("/")
            state = ls2[2][-2:]
            city = ls2[2][:-2]
            newlist = [city, state, ls2[3:7], mpg_split, ls2[8], carname]
            ls2 = ls2[0:2]
            ls2.extend(newlist)
            ls2 = flatten(ls2)
            print ls2
            with open(r"scrape_output.csv", 'a') as f:
                writer = csv.writer(f)
                writer.writerow(ls2)
