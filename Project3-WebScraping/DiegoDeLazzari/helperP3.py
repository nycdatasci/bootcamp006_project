# -*- coding: utf-8 -*-
"""
Created on Sat Aug 13 21:12:30 2016

@author: Diego
"""
from selenium import webdriver
from bs4 import BeautifulSoup # For HTML parsing
from time import sleep # To prevent overwhelming the server between connections
from collections import Counter # Keep track of our term counts
from nltk.corpus import stopwords # Filter out stopwords, such as 'the', 'or', 'and'
import pandas as pd # For converting results to a dataframe and bar chart plots
from selenium.webdriver.common import action_chains, keys
from selenium.common.exceptions import NoSuchElementException
import numpy as np
import pickle
import re

def init_glassdoor():
    ''' Initialize chrome driver'''
    
    chrome_options = webdriver.ChromeOptions()
    
    driver = '/Users/Diego/Documents/NYCDSA/Project 3/chromedriver'
    
    chrome_options.add_argument('--disable-extensions')
    chrome_options.add_argument('--profile-directory=Default')
    chrome_options.add_argument("--incognito")
    chrome_options.add_argument("--disable-plugins-discovery")
    chrome_options.add_argument("--start-maximized")
    browser = webdriver.Chrome(driver, chrome_options=chrome_options)

    return browser

##############################################################################

def searchJobs(jobName, city=None, jobDict = None, link=None):
    '''Scrape for job listing'''

    q = raw_input('Shall we scrape? (y/n)')
    
    if q=='y':
        
        job = browser.find_element_by_id("KeywordSearch")
        location = browser.find_element_by_id("LocationSearch")
        sleep(3)
        #browser.execute_script("arguments[0].value = ''", job)
        job.send_keys(jobName)
        sleep(2)
        browser.execute_script("arguments[0].value = ''", location)
        location.send_keys(city)
        
        sleep(2)
        
        browser.execute_script("document.querySelectorAll('button.gd-btn')[1].click()")
        sleep(5)

        # Set up starting page 
        initial_url = browser.current_url
    
        
        # Find brief description
        
        
        for i in range(20):
            try:
                # Extract useful classes
                jobPosting =browser.find_elements_by_class_name('jobListing')
                sleep(get_pause())
                # Create basic job Dictionary - but first check whether the keys exist    
                newPost = filter(lambda b: b[0] not in jobDict.keys(),
                            map(lambda a: (a.get_attribute('data-id'), a), jobPosting))
                
                if newPost != []:
    
                    # then process the tuple and update the dictionary
                    jobData =  map(lambda (a,b): (a,b.text.encode("utf8").split('\n')[0:4]),newPost)
                    
                    # Update Dictionary
                    tmp = map(lambda a: jobDict.update({a[0]:a[1]}),jobData)    
                    
                    # finally find the links: make a tuple with id so that you can use the Dict
                    link_lst = map(lambda (c,d): (c,d.find_element_by_tag_name('a'). \
                           get_attribute('href')), newPost)
                    tmp = map(lambda (c,d): jobDict[c].append(d),link_lst)
                    
                    # update link
                    link += link_lst

                try:
                    browser.find_element_by_class_name('mfp-close').click()
                except:
                    pass
                
                browser.find_element_by_class_name('next').click()
                    
            except:
                pass
            
    return jobDict, link
    
###############################################################################    

def text_cleaner(text):
    '''
    This function just cleans up the raw html so that I can look at it.
    Inputs: a URL to investigate
    Outputs: Cleaned text only
    '''
    
    
    lines = (line.strip() for line in text.splitlines()) # break into lines
    
        
    chunks = (phrase.strip() for line in lines for phrase in line.split("  ")) # break multi-headlines into a line each
    
    def chunk_space(chunk):
        chunk_out = chunk + ' ' # Need to fix spacing issue
        return chunk_out  
        
    
    text = ''.join(chunk_space(chunk) for chunk in chunks if chunk).encode('utf-8') # Get rid of all blank lines and ends of line
        
        
    # Now clean out all of the unicode junk (this line works great!!!)
        
    try:
        text = text.decode('unicode_escape').encode('ascii', 'ignore') # Need this as some websites aren't formatted
    except:                                                            # in a way that this works, can occasionally throw
        return                                                         # an exception
       
        
    text = re.sub("[^a-zA-Z.+3]"," ", text)  # Now get rid of any terms that aren't words (include 3 for d3.js)
                                                # Also include + for C++
        
       
    text = text.lower()  # Go to lower case
    
#     # find experience
#    try:
#        experience = re.search('(\d+).*(years).*(experience)',text).groups()
#        exp = ' '.join(experience)
#    except:
#        pass
    
    text = text.split()  #  and split them apart
        
        
    stop_words = set(stopwords.words("english")) # Filter out any stop words
    text = [w for w in text if not w in stop_words]
        
        
        
    text = list(set(text)) # Last, just get the set of these. Ignore counts (we are just looking at whether a term existed
                            # or not on the website)
        
    return text 
    
    
##############################################################################
def get_match(cv,jDict):
    '''Use the Jaccard similarity measure to choose the best match for a CV'''

    # Parse cv according to predifined categories
    new_cv = skills_info(cv)
    
    # Flatten, filter and update the dict
#    score =  map(lambda (x,y): [x, jDict[x][2], jDict[x][3], Jaccard(new_cv,y)], 
#                 map(lambda (x,y): (x, skills_info([y[0]]+y[5])), jDict.items()))
    score =  map(lambda (x,y): [x, jDict[x][2], jDict[x][3], Jaccard(new_cv,y[5])],jDict.items())
    
                 
    best = pd.DataFrame(score, columns=['Id','Company','Location','Similarity'])\
             .sort_values(ascending = False, by='Similarity')
    
    return best        
    
##############################################################################
    
def skills_info(jDict):
    
    
    doc_frequency = Counter() # This will create a full counter of our terms. 
        
    
    if isinstance(jDict,dict):
        
        try:
            [doc_frequency.update(jDict[item][5]) for item in jDict.keys()] # List comp
        except:
            doc_frequency.update(jDict[jDict.keys()][5])
            
    else:        
        doc_frequency.update(jDict)
            
    # Now we can just look at our final dict list inside doc_frequency
    
    # Obtain our key terms and store them in a dict. These are the key data science skills we are looking for
    
    prog_lang_dict = Counter({'R':doc_frequency['r'], 'Python':doc_frequency['python'],
                    'Java':doc_frequency['java'], 'C++':doc_frequency['c++'],
                    'Ruby':doc_frequency['ruby'], 'Julia':doc_frequency['julia'],
                    'Perl':doc_frequency['perl'], 'Matlab':doc_frequency['matlab'], 
                    'Mathematica':doc_frequency['mathematica'], 'Php':doc_frequency['php'],
                    'JavaScript':doc_frequency['javascript'], 'Scala': doc_frequency['scala']})
                      
    analysis_tool_dict = Counter({'Excel':doc_frequency['excel'],  'Tableau':doc_frequency['tableau'],
                        'D3.js':doc_frequency['d3.js'], 'SAS':doc_frequency['sas'],
                        'SPSS':doc_frequency['spss'], 'D3':doc_frequency['d3']})  

    hadoop_dict = Counter({'Hadoop':doc_frequency['hadoop'], 'MapReduce':doc_frequency['mapreduce'],
                'Spark':doc_frequency['spark'], 'Pig':doc_frequency['pig'],
                'Hive':doc_frequency['hive'], 'Shark':doc_frequency['shark'],
                'Oozie':doc_frequency['oozie'], 'ZooKeeper':doc_frequency['zookeeper'],
                'Flume':doc_frequency['flume'], 'Mahout':doc_frequency['mahout']})
    
    other_dict = Counter({'Azure':doc_frequency['azure'], 'AWS':doc_frequency['aws']})
                
    database_dict = Counter({'SQL':doc_frequency['sql'], 'NoSQL':doc_frequency['nosql'],
                    'HBase':doc_frequency['hbase'], 'Cassandra':doc_frequency['cassandra'],
                    'MongoDB':doc_frequency['mongodb']})
                    
    # About Education         
    
    edu_dict = Counter({'Bachelor':doc_frequency['bachelor'],'Master':doc_frequency['master'],\
                          'PhD': doc_frequency['phd'],'MBA':doc_frequency['mba']})
                          
    # Calculate requested languages other than english
    lang_dict = Counter({'French':doc_frequency['french'],'German':doc_frequency['german'],
                         'Spanish':doc_frequency['spanish'],'Chinese':doc_frequency['chinese']})
          
    # Education counters 2 Useless for analitycs but usefull for model
    education_dict = Counter({'Computer Science':doc_frequency['computer-science'],  
                              'Statistics':doc_frequency['statistics'], 
                              'Mathematics':doc_frequency['mathematics'],
                              'Physics':doc_frequency['physics'], 
                              'Machine Learning':doc_frequency['machine-learning'], 
                              'Economics':doc_frequency['economics'], 
                              'Software Engineer': doc_frequency['software-engineer'],
                              'Information System':doc_frequency['information-system'], 
                              'Quantitative Finance':doc_frequency['quantitative-finance']})

    overall_total_skills = prog_lang_dict + analysis_tool_dict + hadoop_dict + database_dict  + other_dict# Combine our Counter objects

    skl_dict =  education_dict + lang_dict +  edu_dict + overall_total_skills
    
    if len(jDict) > 1 and isinstance(jDict,dict):
                  
        skill_frame = pd.DataFrame(overall_total_skills.items(), columns = ['Term', 'NumPostings']) # Convert these terms to a 
                                  
        edu_frame = pd.DataFrame(edu_dict.items(), columns = ['Term', 'NumPostings']) # Convert these terms to a 
        
        lang_frame = pd.DataFrame(lang_dict.items(), columns = ['Term', 'NumPostings'])
        
        return skill_frame, edu_frame, lang_frame
       
    else: 
        skl_lst = filter(lambda x: skl_dict[x] >= 1,skl_dict.keys())
        
        return skl_lst

###############################################################################
        
def checkDict(jDict, link):
        
    # 1- Check number of fields 
    bad_key = filter(lambda x: len(jDict[x]) < 5 ,jDict.keys())
    # 2- Delete bad keys
    keys_removed = [jDict.pop(ind) for ind in bad_key]
    
    
    bad_value = filter(lambda x: len(jDict[x]) > 6,jDict.keys())
    
    values_removed = [jDict[ind].pop(6) for ind in bad_value]
    
        
    # 2b - Revove key = none
    if None in jDict:
        del jDict[None]
    
    # 3- filter links whose keys are in Dict ONLY if the bag of words is missing
    link = filter(lambda x: (x[0] in jDict) and (len(jDict[x[0]]) == 5), link)  

    print '###################################################'
    print 'Lenght of Dictionary is ' + str(len(jDict))
    print 'Lenght of link_lst is ' + str(len(link))
    print 'Number of bags of words is ' + str(len(jDict) - len(link))
    print '###################################################'
    
    save = raw_input('Shall I save? (y/n)')
    
    if save == 'y':
        
        save_obj(jDict, 'glassDoorDict')
        save_obj(link, 'glassDoorlink')
        
    return jDict, link
    
###############################################################################
    
def get_pause():
    return  np.random.choice(range(4,6)) 

############################################################################### 
        
def save_obj(obj, name ):
    with open(name + '.pkl', 'wb') as f:
        pickle.dump(obj, f, pickle.HIGHEST_PROTOCOL)
        
############################################################################### 
        
def load_obj(name ):
    with open(name + '.pkl', 'rb') as f:

        return pickle.load(f)     
###############################################################################

def Jaccard(x,y):
    """returns the jaccard similarity between two lists """
     
    intersection_cardinality = len(set.intersection(*[set(x), set(y)]))
    union_cardinality = len(set.union(*[set(x), set(y)]))
    
    return intersection_cardinality/float(union_cardinality)

        

