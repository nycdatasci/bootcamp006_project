# -*- coding: utf-8 -*-
"""
Created on Thu Sep 15 17:06:28 2016

@author: Diego
"""
import pandas as pd
import numpy as np
import time
import csv
from collections import defaultdict

# What do you want to process?
is_training = raw_input('What do you want to process? 0=Training, 1=Test')

start = time.time()

# FUN
def dateFun(row):
    try:
        return max(row) - min(row)
    except:
        return None
#        
if is_training == '0':
    f = open('Train/train_date.csv', 'rb')
    g = open('Train/train_numeric.csv', 'rb')
    h = open('Train/train_categorical.csv', 'rb')
    
    outputDate = pd.DataFrame(index = range(1183747), columns = ['Id', 'TimeLapse', 'DateStep', 'NumStep', 'CatStep','Response'])


elif is_training == '1':
    f = open('Test/test_date.csv', 'rb')
    g = open('Test/test_numeric.csv', 'rb')
    h = open('Test/test_categorical.csv', 'rb')
    
    outputDate = pd.DataFrame(index = range(1183747), columns = ['Id', 'TimeLapse', 'DateStep', 'NumStep', 'CatStep'])
    
dateTmp  = csv.reader(f)
numeTmp  = csv.reader(g)
cateTmp  = csv.reader(h)

headDate = dateTmp.next() 
headNume = numeTmp.next() 
headCate = cateTmp.next()


for i in xrange(len(outputDate)): # 100,000 => use 10% of the data => 1.5 minutes

    date = map(lambda b: float(b),filter(lambda a: a!='',dateTmp.next()))
    num  = map(lambda b: float(b),filter(lambda a: a!='', numeTmp.next()))
    cat  = filter(lambda a: a!='', cateTmp.next())
    if is_training == '0': 
        outputDate.values[i,:] = [date[0], dateFun(date[1:]),len(date)-1, len(num)-2, len(cat)-1, num[-1]]
    elif is_training == '1':
        outputDate.values[i,:] = [date[0], dateFun(date[1:]),len(date)-1, len(num)-2, len(cat)-1]

end = time.time()
if is_training == '0':
    outputDate.to_csv('Train/outputDate.csv') 
elif is_training == '1':
    outputDate.to_csv('Test/outputDate.csv') 
    
f.close()
g.close()
h.close()
print outputDate
print (end - start)/60
#    return outputDate
#pd.read_csv()