'''
------------------------------------------------------------------------------
Creator: Bernard Ong
Created: Aug 2016
Project: Web Scraping Project
Purpose: simple ebook title recommended
------------------------------------------------------------------------------
'''

import re
import nltk
import string
import pandas as pd
from nltk.corpus import stopwords
from nltk.tokenize import RegexpTokenizer

class ReviewCorpus:
    corpus = dict()

    def remove_non_ascii(self, text):
        # removes all non-ascii characters
        return ''.join([i if ord(i) < 128 else '' for i in text])

    def cleanupTitle(self, s):
        # remove stopwords
        stopset = set(stopwords.words('english'))
        punctuations = list(string.punctuation)
        tokens = [i for i in nltk.word_tokenize(re.sub(r'\d+', '', s.lower())) if i not in punctuations]
        cleanup = " ".join(filter(lambda word: word not in stopset, tokens))
        cleanup = self.remove_non_ascii(cleanup)
        cleanup = cleanup.replace('...','')
        cleanup = cleanup.replace("'s",'')
        cleanup = cleanup.replace("''",'')
        cleanup = cleanup.replace("``",'')
        cleanup = cleanup.replace("-",'')
        cleanup = cleanup.replace("''",'')
        cleanup = cleanup.replace("'",'')
        return cleanup

    def cleanTokens(self, s):
        return nltk.word_tokenize(self.cleanupTitle(s))

    def add(self, title, rating):
        newsent = self.cleanTokens(title)
        for x in range(len(newsent)):
            if newsent[x] not in self.corpus:
                self.corpus[newsent[x]] = [0,0,0,0,0]
            if rating > 0.00 and rating <= 1.44: self.corpus[newsent[x]][0] += 1
            if rating > 1.44 and rating <= 2.44: self.corpus[newsent[x]][1] += 1
            if rating > 2.44 and rating <= 3.44: self.corpus[newsent[x]][2] += 1
            if rating > 3.44 and rating <= 4.44: self.corpus[newsent[x]][3] += 1
            if rating > 4.44 and rating <= 5.00: self.corpus[newsent[x]][4] += 1

if __name__ == '__main__':
    # this part is not used in the main scraper, but used only for development and debugging purposes
    rc = ReviewCorpus()
    rc.add('What\'s a Cook to Do?: An Illustrated Guide to 484 Essential Tips, Techniques, and Tricks', 3.5)
    print rc.corpus
