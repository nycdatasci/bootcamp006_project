#!/usr/bin/env python3

import requests
from bs4 import BeautifulSoup
import csv	
import re
import pandas as pd
import sys # used to dump the whole dataframe 

r = requests.get('https://www.statcrunch.com/grabdata.php?dataid=1096769&_=1470977876486') # making GET request

data = r.text.splitlines()

headers = data.pop(0).split() # extracting headers


with open('statcrunch_dump.csv', 'wb') as write_file: # file to which data will be written
	writer = csv.writer(write_file)
	for line in data:
		# extracting values
		name = re.split("\" ", line)[0].strip('"')
		data = re.split("\" ", line)[1].split()

		# combining data
		meta = data[:0] + [name] + data[0:]
		
		meta = [d.encode('utf-8') for d in meta] # reason for encoding: remove u
		
		writer.writerow(meta) # writing to file


df = pd.read_csv('statcrunch_dump.csv') # inserting in pandas
print df.to_csv(sys.stdout) # printing from pandas