'''
------------------------------------------------------------------------------
Creator: Bernard Ong
Created: Aug 2016
Project: Web Scraping Project
Purpose: Scraper Code for Amazon Kindle eBook Site
------------------------------------------------------------------------------
'''

# import libraries
import pandas as pd
from rc import ReviewCorpus
from amazon import AmazonKindle
import sys

# get the command line argument for the Amazon search
query = sys.argv[1]

# initialize the variables to capture price/rating/review (prr) and review corpus
prr = []
rc = ReviewCorpus()
titles = []

# instantiate class for the Amazon Kindle ebook list based on keywords
amz = AmazonKindle(query)

# execute the class iterator method
# build out the prr collection
for a in amz:
    if not a: pass
    else:
        title, price, rating, review = a
        rc.add(title, rating)
        prr.append([price, rating, review])
        titles.append(rc.cleanupTitle(title))

# build out the ebook list with frequency count of each rating
rev_corp = []
for key, val in rc.corpus.iteritems():
    key = rc.cleanupTitle(key)
    if key.strip() != "": rev_corp.append([key] + val)

# Convert to dataframe and Export Data to csv files
df_revcorp = pd.DataFrame(rev_corp)
df_prr = pd.DataFrame(prr)
df_titles = pd.DataFrame(titles)

# convert dataset to csv format for export readiness
rc_data = df_revcorp.to_csv(index=False, header=['title', 'tr1', 'tr2', 'tr3', 'tr4', 'tr5'])
pr_data = df_prr.to_csv(index=False, header=['price', 'rating', 'review'])
titles_data = df_titles.to_csv(index=False)

# opne the data files for writing
csv_revcorp = open(query.replace(' ', '_') + '_revcorp.csv', 'w')
csv_prr = open(query.replace(' ', '_') + '_prr.csv', 'w')
csv_titles = open(query.replace(' ', '_') + '_titles.csv', 'w')

# write the entire dataset  to file
csv_revcorp.write(rc_data)
csv_prr.write(pr_data)
csv_titles.write(titles_data)

# close the files
csv_revcorp.close()
csv_prr.close()
csv_titles.close()
