# -*- coding: utf-8 -*-
"""
Created on Sat Sep 10 17:21:02 2016

@author: venkatesh
"""

import requests
import os


class SocrataConnection(object):

    def __init__(self, url, token, limit):
        self.url = url
        self.token = token
        self.limit = limit
        self.headers = sorted(self._headers())
        self.primary_id = None

    def _headers(self):
        query = '{0}/?$limit=1'.format(self.url)
        r = requests.get(query)
        return r.json()[0].keys()

    def get_headers(self):
        return self.headers

    def get_rowcount(self):
        if self.primary_id is None:
            print 'Please set primary id first. (Use .set_primary_id("id")'
            return None

        query = '{0}?$select=count({1})'.format(self.url, self.primary_id)
        result_field = 'count_{0}'.format(self.primary_id)
        r = requests.get(query)
        return int(r.json()[0][result_field])

    def set_primary_id(self, primary_id):
        if primary_id in self.headers:
            self.primary_id = primary_id
            self.headers.remove(primary_id)
            self.headers.insert(0, primary_id)
        else:
            print 'Column does not exist'

    def download_csv(self, rows=None, file_name='untitled.csv', headers=True):
        if rows is None:
            rows = self.get_rowcount()

        if self.primary_id is None:
            print 'Please set primary id first. (Use .set_primary_id("id")'
            return None

        if os.path.exists(file_name):
            os.remove(file_name)

        if headers:
            self._write_csv_row(dict(zip(self.headers, self.headers)),
                                file_name)

        if self._write_to_csv(rows, file_name):
            print 'Download Complete'
        else:
            print 'Download Incomplete'


    def _write_to_csv(self, rows, file_name):
        offset = 0
        link = '{0}?$$app_token={1}&$order={2} DESC&$limit={3}&$offset={4}'
        for i in xrange((rows / self.limit) + 1):
            query = link.format(self.url, self.token, self.primary_id,
                                self.limit, offset)
            r = requests.get(query)
            if r.status_code == 200:
                for row in r.json():
                    self._write_csv_row(row, file_name)
                offset += self.limit
            else:
                return False
        return True

    def _write_csv_row(self, row, file_name):
        data = []
        for col in self.headers:
            try:
                item = '"' + str(row[col]).strip() + '"'
            except:
                item = ' '
            data.append(item)
        with open(file_name, 'a') as out_file:
            out_file.write(','.join(data) + ' \n')


class SocrataAPI(object):

    def __init__(self, token, limit=50000):
        self.token = token
        self.limit = limit

    def request(self, url, show_details=True):
        return SocrataConnection(url, self.token, self.limit)
        
token = 'Opp5gz1KaGplrPjbqnSsWkqHB'
api = SocrataAPI(token)
r = api.request('https://data.seattle.gov/resource/pu5n-trf4.json')
headers = r.get_headers()
r.set_primary_id('cad_cdw_id')
r.download_csv(file_name='raw_911_response.csv')
