import csv
import numpy as np
from nltk.sentiment.vader import SentimentIntensityAnalyzer
from nltk import tokenize

name = []
review = []

sid = SentimentIntensityAnalyzer() 

target = open("output_1.csv", 'w') 

target.write("Name")
target.write(",")
target.write("Review")
target.write(",")
target.write("compound")
target.write(",")
target.write("positive")
target.write(",")
target.write("neutral")
target.write(",")
target.write("negative")
target.write("\n")

with open('daa.csv') as csvfile:
    reader = csv.DictReader(csvfile)
    
    for row in reader:
        name = row['key']
        lines_list = tokenize.sent_tokenize(row['value'])
        
        pos_rate = []
        neu_rate = []
        neg_rate = []
        compound_rate = []
        
        for sentence in lines_list:
            ss = sid.polarity_scores(sentence)
            compound_rate = ss['compound']
            pos_rate = ss['pos']
            neu_rate = ss['neu']
            neg_rate = ss['neg']
        
        compound = np.mean(compound_rate)
        pos = np.mean(pos_rate)
        neg = np.mean(neg_rate)
        neu = np.mean(neu_rate)
            
        target.write(row['key'])
        target.write(",")
        target.write(row['value'])
        target.write(",")
        target.write(str(compound))
        target.write(",")
        target.write(str(pos))
        target.write(",")
        target.write(str(neu))
        target.write(",")
        target.write(str(neg))
        target.write("\n")
        
        print(compound,pos,neg,neu)

target.close()



 
