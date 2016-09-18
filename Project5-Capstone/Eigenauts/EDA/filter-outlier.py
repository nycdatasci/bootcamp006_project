# -*- coding: utf-8 -*-
"""
Created on Tue Sep 06 15:41:19 2016

This file deal with the outlier. 
Here is how it works:
  Step1: Create three functions--- mad_based_outlier, percentile_based_outlier, std_outlier to identify the outlier
         based on three different methods. 
  Step2: Create a function called outlierVote. Whenever pass in the data, we do a vote on it. If more than 2 out 3
         method say that the data is a outlier, we will apply functions created in step 3 to replace the outlier.
  Step3: Create functions to handle different cases of outliers for each column.
         For variables NumofTime30-59DaysPastDue,NumberOfTime6089DaysPastDueNotWorse, NumofTime90DaysPastDue, create function
         removeSpecificAndPutMedian to replace those cases equal 98 or 96 with median of the column.
         Create function replaceOutlier to replace outlier with either the median or the minupper(upper bound of the outlier detection).
         For variable RevolvingUtilizationOfUnsecuredLines,age, DebtRatio, NumberOfDependents, NumberRealEstateLoansOrLines, 
         replace outlier with minupper.
         For variable Monthly Income, replace the ourlier with median.
         

@author: my125
"""

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from collections import Counter

data = pd.read_csv('cs-training.csv').drop('Unnamed: 0', axis = 1)

cleanCol = []
for i in range(len(data.columns)):
    cleanCol.append(data.columns[i].replace('-', ''))
    
data.columns = cleanCol

print data.columns

####Detect outlier#####
print 'Start Outlier detection'
def mad_based_outlier(points, thresh=3.5):
    if len(points.shape) == 1:
        points = points[:,None]
    median = np.nanmedian(points, axis=0)
    print median
    diff = np.nansum((points - median)**2, axis=-1)
    diff = np.sqrt(diff)
    med_abs_deviation = np.median(diff)

    modified_z_score = 0.6745 * diff / med_abs_deviation

    return modified_z_score > thresh

def percentile_based_outlier(data, threshold=95):
    diff = (100 - threshold) / 2.0
    (minval, maxval) = np.percentile(data.dropna(), [diff, 100 - diff])
    return ((data < minval) | (data > maxval))


def std_div(data, threshold=3):
    std = data.dropna().std()
    mean = data.dropna().mean()
    isOutlier = []
    for val in data:
        if val/std > threshold:
            isOutlier.append(True)
        else:
            isOutlier.append(False)
    return isOutlier

def outlierVote(data):
    x = percentile_based_outlier(data)
    y = mad_based_outlier(data)
    z = std_div(data)
    temp = zip(data.index, x, y, z)
#    print data.index,x,y,z
    final = []
        
    for i in range(len(temp)):
        if temp[i].count(False) >= 2:
            final.append(False)
        else:
            final.append(True)
    print any(final)
    return final


def plotOutlier(x):
    fig, axes = plt.subplots(nrows=4)
    for ax, func in zip(axes, [percentile_based_outlier, mad_based_outlier, std_div, outlierVote]):
        sns.distplot(x, ax=ax, rug=True, hist=False)
        outliers = x[func(x)]
        ax.plot(outliers, np.zeros_like(outliers), 'ro', clip_on=False)

    kwargs = dict(y=0.95, x=0.05, ha='left', va='top', size=20)
    axes[0].set_title('Percentile-based Outliers', **kwargs)
    axes[1].set_title('MAD-based Outliers', **kwargs)
    axes[2].set_title('STD-based Outliers', **kwargs)
    axes[3].set_title('Majority vote based Outliers', **kwargs)
    fig.suptitle('Comparing Outlier Tests with n={}'.format(len(x)), size=20)
    fig = plt.gcf()
    fig.set_size_inches(15,10)
    
def plotOutlierFree(x):
    fig, axes = plt.subplots(nrows=4)
    nOutliers = []
    for ax, func in zip(axes, [percentile_based_outlier, mad_based_outlier, std_div, outlierVote]):
        tfOutlier = zip(x, func(x))
        nOutliers.append(len([index for (index, bol) in tfOutlier if bol == True]))
        outlierFree = [index for (index, bol) in tfOutlier if bol == True]
        sns.distplot(outlierFree, ax=ax, rug=True, hist=False)
        
    kwargs = dict(y=0.95, x=0.05, ha='left', va='top', size=15)
    axes[0].set_title('Percentile-based Outliers, removed: {r}'.format(r=nOutliers[0]), **kwargs)
    axes[1].set_title('MAD-based Outliers, removed: {r}'.format(r=nOutliers[1]), **kwargs)
    axes[2].set_title('STD-based Outliers, removed: {r}'.format(r=nOutliers[2]), **kwargs)
    axes[3].set_title('Majority vote based Outliers, removed: {r}'.format(r=nOutliers[3]), **kwargs)
    fig.suptitle('Outlier Removed By Method with n={}'.format(len(x)), size=20)
    fig = plt.gcf()
    fig.set_size_inches(15,10)

def outlierRatio(data):
    functions = [percentile_based_outlier, mad_based_outlier, std_div, outlierVote]
    outlierDict = {}
    for func in functions:
        funcResult = func(data)
        count = 0
        for val in funcResult:
            if val == True:
                count += 1 
        outlierDict[str(func)[10:].split()[0]] = [count, '{:.2f}%'.format((float(count)/len(data))*100)]
    
    return outlierDict
    
def removeSpecificAndPutMedian(data, first = 98, second = 96):
    New = []
    med = data.median()
    for val in data:
        if ((val == first) | (val == second)):
            New.append(med)
        else:
            New.append(val)
            
    return New

####Replace outlier with median####
def replaceOutlier(data, method = outlierVote, replace='median'):
    '''replace: median (auto)
                'minUpper' which is the upper bound of the outlier detection'''
    vote = outlierVote(data)

    x = pd.DataFrame(zip(data, vote), columns=['debt', 'outlier'])
    if replace == 'median':
        replace = x.debt.median()
    elif replace == 'minUpper':
        replace = min([val for (val, vote) in zip(data, vote) if vote == True])
        if replace < data.mean():
            return 'There are outliers lower than the sample mean'
    debtNew = []
    for i in range(x.shape[0]):
        if x.iloc[i][1] == True:
            debtNew.append(replace)
        else:
            debtNew.append(x.iloc[i][0])
    
    return debtNew

plotOutlier(data.RevolvingUtilizationOfUnsecuredLines.sample(1000))

#####outlier replace by 2 for RevolvingUtilizationOfUnsecuredLines
revNew = []
for val in data.RevolvingUtilizationOfUnsecuredLines:
    if val <= 2:
        revNew.append(val)
    else:
        revNew.append(2.)

data.RevolvingUtilizationOfUnsecuredLines = revNew
#####age variable####
for i in range(16,30):
    print i, len(data[data.age < i])
    
ageNew = []
for val in data.age:
    if val > 22:
        ageNew.append(val)
    else:
        ageNew.append(22)
        
data.age = ageNew

#####NumofTime30-59DaysPastDue####
Counter(data.NumberOfTime3059DaysPastDueNotWorse)

New = []
med = data.NumberOfTime3059DaysPastDueNotWorse.median()
for val in data.NumberOfTime3059DaysPastDueNotWorse:
    if ((val == 98) | (val == 96)):
        New.append(med)
    else:
        New.append(val)

data.NumberOfTime3059DaysPastDueNotWorse = New

#####DebtRatio#####
outlierRatio(data.DebtRatio)
minUpperBound = min([val for (val, out) in zip(data.DebtRatio, mad_based_outlier(data.DebtRatio)) if out == True])

newDebtRatio = []
for val in data.DebtRatio:
    if val > minUpperBound:
        newDebtRatio.append(minUpperBound)
    else:
        newDebtRatio.append(val)
        
data.DebtRatio = newDebtRatio

####Monthly Income Var####
plotOutlier(data.MonthlyIncome.sample(1000))

incomeNew = replaceOutlier(data.MonthlyIncome, replace='median')
data.MonthlyIncome = incomeNew

####NumofTime90DaysPastDue#####
new = removeSpecificAndPutMedian(data.NumberOfTimes90DaysLate)
data.NumberOfTimes90DaysLate = new

#####NumberRealEstateLoansOrLines ####
realNew = []
for val in data.NumberRealEstateLoansOrLines:
    if val > 17:
        realNew.append(17)
    else:
        realNew.append(val)

data.NumberRealEstateLoansOrLines = realNew

####NumberOfTime6089DaysPastDueNotWorse####
new = removeSpecificAndPutMedian(data.NumberOfTime6089DaysPastDueNotWorse)
data.NumberOfTime6089DaysPastDueNotWorse = new

####NumberOfDependents ####
depNew = []
for var in data.NumberOfDependents:
    if var > 10:
        depNew.append(10)
    else:
        depNew.append(var)

data.NumberOfDependents = depNew

print 'This completes the process'

data.to_csv('./cs-trainig-outlier-12nn.csv', sep=',')

print 'saved file'