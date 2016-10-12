# -*- coding: utf-8 -*-
"""
Created on Sat Sep 10 17:17:21 2016

@author: venkatesh
"""



import requests
from bs4 import BeautifulSoup
import datetime
from dateutil import parser
import re
import pandas as pd
import imp
import os


CWD = os.path.dirname(os.path.abspath(__file__))
DOWNLOAD_PATH = CWD + 'weather.csv'

class WundergroundScraper(object):
    '''
    Takes a designated Wunderground city key and quickly allows you to download
     historical weather information between a range of dates.
    Usage:
    wunder = ws.WundergroundScraper()
    wunder.download_date_range('2009-06-17', '2015-09-29')
    '''

    def __init__(self, city='KBFI'):
        '''
        INPUT:
            city -> string; Wundeground city key
        Initiates the Wunderground scraper class. 
        '''
        self.city = city
        self.url = 'http://www.wunderground.com/history/airport/'\
                   '{a}/{y}/{m}/{d}/DailyHistory.html'
        self.data = []

    def download_date_range(self, start_dt, end_dt, f_path=DOWNLOAD_PATH):
        '''
        INPUT:
            start_dt -> string; start date of scrape
            end_dt -> string; end date of scrape
            f_path -> string; path to save scraped data
        For each date in the given date range, scrape and format the historical
        weather data.  Then save to a csv.
        '''
        end_dt = parser.parse(end_dt)
        start_dt = parser.parse(start_dt)
        diff = end_dt - start_dt
        dates = [end_dt - datetime.timedelta(days=x) \
                          for x in xrange(1, diff.days)]
        for d in dates:
            table = self._make_request(d.year, d.month, d.day)
            header = self._get_header(table)
            self._write_data(str(d.date()), table, header)
        self._save_to_csv(f_path)
        

    def _make_request(self, year, month, day):
        '''
        INPUT:
            year -> int; year of the scrape
            month -> int; month of the scrape
            day -> int; day of the scrape
        OUTPUT:
            soup object; table content of interest from the scrape
        Make url request and retrieves the html text, the returns the table of 
        interest.
        '''
        url = self.url.format(a=self.city, y=year, m=month, d=day)
        r = requests.get(url)
        soup = BeautifulSoup(r.text)
        table = soup.findAll('div', {'id': 'observations_details'})
        return table[0]

    def _get_header(self, table):
        '''
        INPUT:
            table -> soup object; table content of interest from the scrape
        OUTPUT:
            list; column name of headers
        Retrieves and returns the headers of the table of interest.
        '''
        data = ['date']
        for header in table.findAll('th'):
            for h in header.strings:
                if '(' not in h:
                    data.append(h.strip())
        return data

    def _write_data(self, date, table, header):
        '''
        INPUT:
            date -> string; date of table scraped
            table -> soup object; scraped table of interest
            header -> list; header of scraped table of interest
        Retrieves and returns the headers of the table of interest.
        '''
        for row in table.findAll('tr', {'class': 'no-metars'}):
            data = [date]
            for col in row.findAll('td'):
                content = col.text.strip('\n').strip().encode('utf-8')
                data.append(content)
            self._data_to_dict(header, data)

    def _data_to_dict(self, header, row):
        d = dict()
        for i, h in enumerate(header):
            d[h] = row[i]
        self.data.append(d) 

    def _save_to_csv(self, f_path):
        df = pd.DataFrame(self.data)
        df.to_csv(f_path, index=False)
        print '{0} downloaded.'.format(f_path)
        
wunder = WundergroundScraper()
wunder.download_date_range('2009-01-01', '2016-09-01')