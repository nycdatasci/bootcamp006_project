from bs4 import BeautifulSoup
import pandas as pd
import requests
import re

# Set constants
start_url = 'http://www.jungfrau-marathon.ch/en/Ranking_list.html'  # Start URL for scraping

# Get year listing page HTML, and parse using BeautifulSoup
soup = BeautifulSoup(requests.get(start_url).text, 'lxml')

# From listing page: Extract links for each year
all_links = [a_tag.get('href') for a_tag in soup.find_all('a')]  # Extract the href links from <a> tags in the soup
yr_links = [url for url in all_links if re.search(r'/lauf/', url)]  # Keep marathon result links

# Iterate for each year
output = pd.DataFrame({})
for yr_link in yr_links:
    cal_yr = int(re.search('\d\d\d\d', yr_link).group(0))  # Extract calendar year
    if cal_yr not in [2015, 2014, 2013, 2012, 2010]:  # The other years have a completely different results formatting
        continue

    # Get results overview page HTML, and parse using BeautifulSoup
    soup2 = BeautifulSoup(requests.get(yr_link).text, 'lxml')

    # From year page: Extract result links
    all_links = [a_tag.get('href') for a_tag in soup2.find_all('a') if a_tag.get('href') is not None]  # Extract links
    # Filter all links to keep only the result links:
    res_links = [url for url in all_links if re.search(r'[RrAaNnGg]{4}0(?:10|20|91|92).*[.HhTtMmLl]{4,5}', url)]
    res_links = [yr_link + link for link in res_links]  # Complete the URL's

    # Iterate over all results links from one calendar year
    for res_url in res_links:
        # Get results page and parse using BeautifulSoup
        print res_url
        soup3 = BeautifulSoup(requests.get(res_url).text, 'lxml')

        # From results page: Extract font tags
        font2_tags = soup3.find_all('font', {'size': '2'}, re.UNICODE)  # Extract font tags where size is 2
        font2_tags.pop(0)  # Remove header line

        # From results page: Extract information from tags
        schnitt = [line.get_text('||', True).split('||')[-1].split()[-1] for line in font2_tags]
        names = [line.span.get_text().strip() for line in font2_tags]  # Extract names from span tags inside font tags
        place = [line.get_text('||', True).split('||')[0][:-1] for line in font2_tags]  # Extract place
        # The above line gets the text and splits the string on html tags with ||, the True is to strip whitespace,
        # then it is split into a list by ||. The first (0th) item in the list is the place, and the last element
        # of the string is excluded because it is a period.
        birth_year = [re.search('[ ](\d{4})[ ]', line.get_text()).group(0)
                      if re.search('[ ](\d{4})[ ]', line.get_text())
                      else u'0000'
                      for line in font2_tags]  # Extr. birth years
        time = [re.search('(\d:\d\d.\d\d,\d)', line.get_text()).group(0)
                if re.search('(\d:\d\d.\d\d,\d)', line.get_text())
                else u'0:00.00,0'
                for line in font2_tags]  # Extract overall times
        nat = [re.search('(?:\d\d\d\d) ([A-Z]{3})', line.get_text()).group(1)
               if re.search('(?:\d\d\d\d) ([A-Z]{3})', line.get_text())
               else u'NON'
               for line in font2_tags]
        year = [cal_yr] * len(names)
        if re.search('[RrAaNnGg]010', res_url):  # RANG010 for men, RANG020 for women -- 2012-2015
            gender = ['M'] * len(names)
        elif re.search('[RrAaNnGg]091', res_url):  # rang091 for men, rang092 for women -- before 2012
            gender = ['M'] * len(names)
        else:
            gender = ['F'] * len(names)

        # Consolidate the data into a data frame
        data = pd.DataFrame({
            'names': names,
            'place': place,
            'birth_year': birth_year,
            'time': time,
            'nat': nat,
            'gender': gender,
            'year': year,
            'schnitt': schnitt})

        # Transform data
        data = data[data['schnitt'] != '----']  # Filter out those who did not finish or were disqualified
        data['time'] = [int(line[0]) * 60 * 60 +  # Hours
                        int(line[2:4]) * 60 +  # Minutes
                        int(line[5:7]) +  # Seconds
                        float('0.' + line[8])  # Tenths of seconds
                        for line in data['time']]  # Convert string to integer number of seconds
        data['schnitt'] = [int(line[0]) * 60 +  # Minutes
                           int(line[-2:])  # Seconds
                           for line in data['schnitt']]  # Convert string to integer number of seconds
        to_int = ['place', 'birth_year', 'year']  # Create list of columns to convert to integers
        data[to_int] = data[to_int].apply(pd.to_numeric)  # Convert columns to integers
        data.insert(data.shape[1], 'age', [cal_yr - born for born in data['birth_year']])  # Calculate ages

        if len(data):
            output = output.append(data)  # Append collected data to output data frame

# Write the data to a CSV file
output.to_csv('data.csv', encoding='utf-8', index=False)
