import urllib
from bs4 import BeautifulSoup
import re
import random as rand
import pandas as pd

df = pd.DataFrame(columns = ('id','expired', 'loan', 'region', 'country', 'name', 'group', 'gender', 'industry', 'months', 'time_on_kiva', 'borrower_cost', 'deliquency_rate', 'default_rate'))
n = 0
ids = rand.sample(range(1,1124860), 50000)

for id in ids:
    try:
        #Getting HTML Data
        url = 'https://www.kiva.org/lend/' + str(id)
        #url = 'https://www.kiva.org/lend/1114506'
        r = urllib.urlopen(url).read()
        soup = BeautifulSoup(r, 'lxml')

        #Expired
        expired_data = str(soup.find_all('h2', class_= 'green-bolded inline'))
        if 'Expired' in expired_data:
            expired = 1
        else:
            expired = 0

        #Loan Total Amount
        loan_data = str(soup.find_all('div', class_ = 'loan-total'))
        start =  re.search('Total loan: \$', loan_data).end()
        end = re.search('</div>]', loan_data).start() - 6
        loan = float(loan_data[start:end].replace(',', ''))


        #Location and Industry Type
        country_data = str(soup.find_all('a', class_ = 'black-underlined', href = '#country-section'))
        start = re.search('\#country-section">', country_data).end()
        end = re.search('</a>]', country_data).start()
        loc = country_data[start:end].split(', ')
        if len(loc) == 1:
            region = 'n/a'
            country = loc[0]

        elif len(loc) >= 2:
            region = loc[0]
            country = loc[-1]
        #Name
        name_data = str(soup.find_all('h1', class_= 'borrower-name'))
        start = re.search('class="borrower-name">', name_data).end()
        end = re.search('</h1>', name_data).start()
        name = name_data[start:end]

        #Group
        if 'group' in name or 'Group' in name:
            group = 1
        else:
            group = 0
        #Gender (experimental)
        gender_data = str(soup.find_all(class_= 'loan-use'))
        start = re.search('n        ', gender_data).end()
        end = re.search('n    </h2>', gender_data).start() -1

        if group == 0:
            if ' he ' in gender_data[start:end] or 'his' in gender_data[start:end]:
                gender = 0
            elif 'she' in gender_data[start:end] or 'her' in gender_data[start:end]:
                gender = 1
            else:
                gender = 2
        else:
            gender = 2


        #Industry
        industry_data = str(soup.find_all('div', class_= 'country-text columns small-10'))
        start = re.search('</a> / ', industry_data).end()
        end = re.search('        </div>', industry_data).start() - 2
        industry = industry_data[start:end]

        #Loan Length
        ll_data = str(soup.find_all('div', class_ = 'green-bolded'))
        start = re.search('"green-bolded">', ll_data).end()
        end = re.search('s</div>,', ll_data).start() + 1
        months = float(str.split(ll_data[start:end], ' ')[0])


        #Partner Time on Kiva
        time_data = str(soup.find_all('div', id = 'partner-time-on-kiva'))
        start = re.search('n                    ', time_data).end()
        end = re.search('n                </', time_data).start() - 1
        time_on_kiva = float(str.split(time_data[start:end], ' ')[0])


        #Partner Average Cost to Borrower
        cost_data = str(soup.find_all('div', id = 'partner-avg-cost-to-borrower'))
        start = re.search('n                    ', cost_data).end()
        end = re.search('n                </', cost_data).start() - 1
        borrower_cost = float(str.split(cost_data[start:end], '% ')[0])

        #Partner Delinquency Rate
        deliquency_data = str(soup.find_all('div', id = 'partner-delinquency-rate'))
        start = re.search('n                    ', deliquency_data).end()
        end = re.search('n                </strong>', deliquency_data).start() - 2
        deliquency_rate = float(deliquency_data[start:end])

        #Partner Default Rate
        default_data = str(soup.find_all('div', id = 'partner-default-rate'))
        start = re.search('n                    ', default_data).end()
        end = re.search('n                </strong>', default_data).start() - 2
        default_rate =  float(default_data[start:end])

        data_point = [id,expired, loan, region, country, name, group, gender, industry, months, time_on_kiva, borrower_cost, deliquency_rate, default_rate]

        n += 1

        df.loc[n] = data_point

        with open('/Users/cholmes/Desktop/data.csv', 'a') as f:
            f.write(str(data_point) + '\n')
        f.close

        print data_point, n
    except AttributeError:
        print 'Atrribute Error!', id
        pass
    except ValueError:
        print 'Value Error!', id

#To use if you want to upload data in one big clump instead of line by line
#df.to_csv('/Users/cholmes/Desktop/' + str(id), header=True, index=None)
