'''
------------------------------------------------------------------------------
Creator: Bernard Ong
Created: Aug 2016
Project: Web Scraping Project
Purpose: The Best Words to Use to Title Your Book - Using Doc2Vec
------------------------------------------------------------------------------
'''

import sys
import csv
import random
import numpy as np
import pandas as pd
from gensim.models.doc2vec import TaggedDocument, Doc2Vec

# Initialize utf-8 Default Encoding
reload(sys)
sys.setdefaultencoding('utf-8')

class LabeledTitle(object):
    def __init__(self, tokens, tag):
        self.title = TaggedDocument(words=tokens, tags=[tag])
        self.tag = tag

class TitleList(object):
    def __init__(self, titles=None):
        if titles is None: self.titles = []
        else: self.titles = titles

    def append(self, title_obj):
        self.titles.append(title_obj)

    def toArray(self):
        titleList_arr = []
        for t in self.titles: titleList_arr.append(t.title)
        return titleList_arr

    def __iter__(self):
        for t in self.titles: yield t

    def size(self):
        return len(self.titles)

# Import Dataset
data = open(sys.argv[1], 'r')

# Convert to Structured List
tList = TitleList()
for index, title in enumerate(data.read().split('\n')):
    tList.append(LabeledTitle(title.split(' '), 'title_'+str(index)))
print 'Processed: ' + str(tList.size()) + " titles in prep for vectoring"

# [Doc2Vec Parameters]
vec_dim = 30
win_size = 1
alpha = 0.01
np.random.seed(381888)
min_count = 2
sample = 1e-5
workers = 100
epochs = 10

# build
model = Doc2Vec(size=int(vec_dim), window=win_size, alpha=alpha, min_count=min_count, sample=sample, workers=workers)
model.build_vocab(tList.toArray())
tmpList = tList.toArray()
print 'Training Model...'
for e in range(epochs):
    print '\tEpoch ' + str(e)
    random.shuffle(tmpList)
    model.alpha -= 0.001
    model.min_alpha = model.alpha

# Inference Test - hardcoded for now
inferred = model.infer_vector(["test1","test2"], steps=15)
similar_vec = model.similar_by_vector(inferred, topn=200)

# convert the similar words found into set format
top200_vec = set(map(lambda x: x[0], similar_vec))
print pd.DataFrame(list(top200_vec),columns=["words"])[:35]
top200_count = open('top200_count.csv', 'r')
top200_count = set([word for word in top200_count.read().split('\n')])

# show the intersection set of words (resulting as set format)
intersect = top200_count.intersection(top200_vec)
intersect = pd.DataFrame(list(intersect),columns=["words"])
wordscore = pd.read_csv('top200_countscores.csv',sep=',',names=["words","score"],skiprows=[0])
final_list = pd.merge(intersect,wordscore).sort_values("score",ascending=False)[0:25].reset_index().drop("index",axis=1)
print final_list
