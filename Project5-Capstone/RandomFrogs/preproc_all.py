# -*- coding: utf-8 -*-
"""
Created on Fri Sep 16 15:35:07 2016

@author: Diego
"""

# -*- coding: utf-8 -*-
""

import pandas as pd
import time
import csv
from collections import Counter

# What do you want to process?
is_training = raw_input('What do you want to process? 0=Training, 1=Test')

# FUN
def construct_line( label, line ):
	new_line = []
	if float( label ) == 0.0:
		label = "0"
	new_line.append( label )

	for i, item in enumerate( line ):
         if item == '' or item == '0':
             continue         
         elif item[0] == 'T':
             item = item[1:]
        
         new_item = "%s:%s" % ( i + 1, item)
         new_line.append( new_item )
	new_line = " ".join( new_line )
	new_line += "\n"
	return new_line

def hashLine(row,uniqueVal):
    
    #get the unique values    
    counter = Counter(uniqueVal)
    # uniqueVal = dict.fromkeys(a, 0)
    counter.update(row)
    return map(lambda x: str(x-1),counter.values()[1:])
    
def uniqueCat(path):
    
    h = open(path, 'rb')
    o = open('Train/uniqueCat.csv', 'wb')
    cateTmp  = csv.reader(h)
    cateTmp.next()
    result = set()

    for row in cateTmp:
        unique_row_items = set(field for field in row[1:])
        for item in unique_row_items:
            result.add(item)
 
    sep = ','
    o.write(sep.join(list(result)[1:]))
    o.close()
    h.close()
    print 'uniqueCat saved'
    return list(result)[1:]
    
####

start = time.time()

# Get unique Categorical values (good vor Test data as well)
try:
    print 'Load uniqueCat'
    g = open('Train/uniqueCat.csv', 'rb')
    g_ = csv.reader(g)
    uniCat = g_.next()
    g.close()
except:    
    print 'Create uniqueCat'
    uniCat = uniqueCat('Train/train_categorical.csv')
    
    
# Get Duplicate columns
print 'Get Duplicate columns'
dupe = pd.read_csv('Kcode/feature_summary.csv')
sele = dupe.groupby(by = 'digest').first().sort_values(by = 'Unnamed: 0')
ind = sele.iloc[:,0].values.tolist()

# Set In
print 'Set Input/Output'
if is_training == '0':
    f = open('Train/outputDate.csv', 'rb')
    g = open('Train/train_numeric.csv', 'rb')
    h = open('Train/train_categorical.csv', 'rb')
    # Output
    o = open( 'Train/train_libsvm_noDup_hashed.data', 'wb' )

elif is_training == '1':
    f = open('Test/outputDate.csv', 'rb')
    g = open('Test/test_numeric.csv', 'rb')
    h = open('Test/test_categorical.csv', 'rb')
    # Output
    o = open('Test/test_libsvm_noDup_hashed.data', 'wb' )
    
dateTmp  = csv.reader(f)
numeTmp  = csv.reader(g)
cateTmp  = csv.reader(h)

headDate = dateTmp.next()
headNume = numeTmp.next() 
headCate = cateTmp.next()

# Loop
print 'Loop over lines'
for i in xrange(1183747): # 100,000 => use 10% of the data => .87 minutes 

    date = dateTmp.next()
    nume = numeTmp.next()
    cate_ = cateTmp.next() 
    
    try:
        cate = hashLine([cate_[j] for j in ind],uniCat)   
    except len(cate) > 93:
        raise()
        
    if is_training == '0':
        line = nume[1:-1] + cate + [date[1], str(float(date[3]) + float(date[4])), date[-1]]
        label = line.pop(-1)
        
    elif is_training == '1':
        # No response is given. Fix the index to 0
        line = nume[1:] + cate + [date[1], str(float(date[3]) + float(date[4]))]
        label = 0
    
    new_line = construct_line( label, line )
    o.write( new_line )
    
end = time.time()

f.close()
h.close()
o.close()

print (end - start)/60