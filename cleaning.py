import csv
import pandas as pd
import statsmodels.formula.api as sm
import numpy as np
import matplotlib.pyplot as plt
import math

#Importing data into pandas (and dropping any duplicates brought in by the scraping method)
df = []
with open('/Users/cholmes/Desktop/data.csv', 'r') as csvfile:
    for i in csv.reader(csvfile):
        df.append(i)
csvfile.close()
headers = df.pop(0)
df = pd.DataFrame(df, columns = headers)
df = df.drop_duplicates()


#Deleting brackets and creating a seperate dataset where strings are numerics
df.id = df.id.str.replace('[', '')
df.default_rate = df.default_rate.str.replace(']', '')
df.industry = df.industry.str.replace("'", '')
df.industry = df.industry.str.replace(" ", '')
df.country = df.country.str.replace("'", '')
df.country = df.country.str.replace(" ", '')

#loading in gdp and continent data
continent = []
with open('/Users/cholmes/Desktop/continent.csv', 'r') as csvfile:
    for i in csv.reader(csvfile):
        continent.append(i)
csvfile.close()
continent
headers = continent.pop(0)
continent = pd.DataFrame(continent, columns = headers)
continent.country = continent.country.str.replace(" ", '')

#gdp = []
#with open('/Users/cholmes/Desktop/gdp.csv', 'r') as csvfile:
#    for i in csv.reader(csvfile):
#        gdp.append(i)
#csvfile.close()
#gdp = pd.DataFrame(gdp, columns = headers)

df = pd.merge(df, continent, on='country')


#Turning Continents into Dummy Variables
Asia = [0] * len(df)
Africa = [0] * len(df)
South_America = [0] * len(df)
Europe = [0] * len(df)
North_America = [0] * len(df)
USA = [0] * len(df)

j = 0
for i in df.continent:
    if i == 'Asia':
        Asia[j] = 1
    elif i == 'Africa':
        Africa[j] = 1
    elif i == 'South America':
        South_America[j] = 1
    elif i == 'Europe':
        Europe[j] = 1
    elif i == 'USA':
        USA[j] = 1
    elif i == 'North America':
        North_America[j] = 1
    j += 1

df['Asia'] = Asia
df['Africa'] = Africa
df['South_America'] = South_America
df['North_America'] = North_America
df['Europe'] = Europe
df['USA'] = USA


df[['id','expired','loan', 'group', 'gender', 'months', 'time_on_kiva', 'borrower_cost', 'deliquency_rate', 'default_rate']] = df[['id','expired','loan', 'group', 'gender', 'months', 'time_on_kiva', 'borrower_cost', 'deliquency_rate', 'default_rate']].apply(pd.to_numeric)

##Summary Statistics for Data
#Percent of observations that are expired
float(sum((df.expired)))/float(len(df))

#Total Expired loans
sum(df.expired)

#Average loan size
sum(df.loan)/len(df.loan)

grouped_gender_mean = df.groupby(['gender']).mean()
grouped_gender_total = df.groupby(['gender', 'expired']).sum()
grouped_gender_total = grouped_gender_total[0:14][0:2]
men = 0
women = 0
other = 0
for i in df.gender:
    if i == 1:
        women += 1
    elif i == 0:
        men += 1
    else:
        other += 1

#Total men, women, groups, and unidentifiables
men, women, sum(df.group), other - men - women - sum(df.group)

#By Country
count_country = df.groupby(['country']).count()
count_country[[0]].sort_values(by = 'id', ascending = False)

grouped_country_mean = df.groupby(['country']).mean()
grouped_country_total = df.groupby(['country']).sum()

#By Industry
count_industry = df.groupby(['industry']).count()
count_industry[[0]].sort_values(by = 'id', ascending = False)

grouped_industry_mean = df.groupby(['industry']).sum()
grouped_industry_total = df.groupby(['industry']).mean()


##Graphs


#Histogram of Countries
x = df.loan

# the histogram of the data
#n, bins, patches = plt.hist(x, 50, normed=1, facecolor='green', alpha=0.75)
plt.hist(x, 500)
plt.xlabel('Loan Size')
plt.ylabel('Number of Loans')
plt.title('Histogram of Kiva Loan Size')
plt.axis([0, 5000, 0, 15000])
plt.grid(True)

#plt.show()

#Bar graph of
menMeans = grouped_gender_mean.loan
menMeans = menMeans.drop(menMeans.index[[2]])

menMeans

N = 3
ind = np.arange(N)  # the x locations for the groups
width = 0.6      # the width of the bars

fig, ax = plt.subplots()
rects1 = ax.bar(ind, menMeans, width, color='b')

# add some text for labels, title and axes ticks
ax.set_ylabel('Mean Loan Size ($)')
ax.set_title('Mean Loan Size by Gender')
ax.set_xticks(ind + width/2)
ax.set_xticklabels(('Males', 'Females', 'Group'))

#plt.show()


#Breaking out by Men vs Women, he=0, she=1, undefined = 2, group = 3
grouped_gender_count = df.groupby(['gender']).count()
grouped_gender_sum = df.groupby(['gender']).sum()

genderexpired = grouped_gender_sum.expired/grouped_gender_count.expired
genderexpired = genderexpired.drop(genderexpired.index[[2]])

N = 3
ind = np.arange(N)  # the x locations for the groups
width = 0.6
fig, ax = plt.subplots()
rects1 = ax.bar(ind, genderexpired, width, color='b')

# add some text for labels, title and axes ticks
ax.set_ylabel('Proportion Expired Loans')
ax.set_title('Expired Loans By Gender')
ax.set_xticks(ind + width/2)
ax.set_xticklabels(('Males', 'Females', 'Group'))

#plt.show()


#Breaking out by continent
grouped_continent_count = df.groupby(['continent']).count()
grouped_continent_sum = df.groupby(['continent']).sum()

continentexpired = grouped_continent_sum.expired/grouped_continent_count.expired

N = 6
ind = np.arange(N)  # the x locations for the groups
width = 0.8
fig, ax = plt.subplots()
rects1 = ax.bar(ind, continentexpired, width, color='b')

# add some text for labels, title and axes ticks
ax.set_ylabel('Proportion Expired Loans')
ax.set_title('Expired Loans By Continent')
ax.set_xticks(ind + width/2)
ax.set_xticklabels(('Africa', 'Asia', 'Europe', 'North America (w/o USA)', 'South America', 'USA'))
fig.autofmt_xdate()

#plt.show()



#Logisitic Regression to fill in gender
import numpy as np
from sklearn import linear_model, metrics, datasets
dataset = df[df['gender'] <= 1]

#Logisitic Regression to fill in gender
model = linear_model.LogisticRegression()

dataset_data = np.array(dataset[['loan', 'months']])
dataset_data = dataset_data.astype(float)
dataset_target = np.array(dataset[['gender']])
dataset_target = dataset_target.ravel()

model.fit(dataset_data,dataset_target)

dataset = df[df['gender'] == 2]
dataset_data = np.array(dataset[['loan', 'months']])

predicted_2 = model.predict(dataset_data)
df_gender = list(df.gender)

j = 0
for i in range(0,len(df_gender)-1):
    if df_gender[i] == 2:
        df_gender[i] = predicted_2[j]
        j += 1
df.gender = df_gender

#Breaking out by Men vs Women, he=0, she=1, undefined = 2, group = 3
df.gender[df.group == 1] = 3
female = [0]*len(df)

for i in range(0,len(df) -1 ):
    if df.gender[i] == 1:
        female[i] = 1
df['female'] = female

#SM requires that intercept be manually entered
intercept = [1] * len(df)
df['intercept'] = intercept

x = df[['months','group', 'female', 'Asia', 'North_America', 'South_America', 'Europe', 'Africa', 'intercept']]
#x = df[['loan', 'group', 'female', 'months', 'default_rate']]
y = np.array(df[['expired']])
y = y.ravel()


coefs = sm.Logit(y, x).fit().params.values
for i in coefs:
    print abs(math.exp(i) - 1)


print sm.Logit(y,x).fit().summary2()















