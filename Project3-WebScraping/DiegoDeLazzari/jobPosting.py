from selenium import webdriver
from bs4 import BeautifulSoup # For HTML parsing
from time import sleep # To prevent overwhelming the server between connections
from collections import Counter # Keep track of our term counts
from nltk.corpus import stopwords # Filter out stopwords, such as 'the', 'or', 'and'
import pandas as pd # For converting results to a dataframe and bar chart plots
from selenium.webdriver.common import action_chains, keys
from selenium.common.exceptions import NoSuchElementException
import numpy as np
import sys


# call the helper

from helperP3 import *

if __name__ == "__main__":
    

    # 1- Load existing dictionary. Check for initial dictionary. 
    # If empty initialize
        
    try:               
        jobDict = load_obj('glassDoorDict')
        link =    load_obj('glassDoorlink')
    except:
        save_obj([], 'glassDoorlink')
        save_obj({}, 'glassDoorDict')
        
        jobDict = load_obj('glassDoorDict')
        link =    load_obj('glassDoorlink')    
    
    # 2- Choose what you want to do: 
#        get_shot => Scraping for links, 
#        get_long => Scraping for data,
#        get_results => Getting analytics

    get_short = False
    get_long = False
    get_result = True
    
    if get_short or get_long:
        
    # 3- initialize website, cities and jobs
        
        website = "https://www.glassdoor.com/index.htm"
        
        jobName_lst = ['Data Scientist', 'Data Analyst']
        jobName = np.random.choice(jobName_lst)
    
        city_lst = ['San Jose','New York','San Francisco','Detroit','Washington','Austin','Boston','Los Angeles',' ']
        city = np.random.choice(city_lst)        
        
        # Initialize the webdriver
        
        browser = init_glassdoor()  
    
    # 4- Scrape the short list or the links (when you ae done, both are false)
    
    
    if get_short:
    
        browser.get(website)
            
        # search for jobs (short description) 
        try:    
                    update_jobDict, update_link = searchJobs(jobName_lst[0], 'US', jobDict, link)
#                    sleep(get_pause())
        except:
            sys.exit("Error message")
            
        # save dictionary and link     
    
        save_obj(update_jobDict, 'glassDoorDict')
        save_obj(update_link, 'glassDoorlink')
        
     # 5- Scrape the job description, for every link
                    
    if get_long:        
        
        while len(link) > 0:
            
             
            try:
                rnd_job = np.random.choice(range(len(link)))
                
                ids = link[rnd_job][0]
                page = link[rnd_job][1]
                
                browser.get(page)                 
                sleep(3)
                
                # Extract text   //*[@id="JobDescContainer"]/div[1]
                desc_list = browser.find_element_by_xpath('//*[@id="JobDescContainer"]/div[1]').text
                description = text_cleaner(desc_list)
                
                # Update dictionary and remove succe
                jobDict[ids].append(description)               
                dummy=link.pop(rnd_job)
                               
                # if everything is fine, save
                save_obj(jobDict, 'glassDoorDict')
                save_obj(link, 'glassDoorlink')
                                                
                print 'Scraped successfully ' + ids
                
                sleep(get_pause())
            except:   
                print ids + ' is not working! Sleep for 10 seconds and retry'
                print 'Still missing ' + str(len(link)) + ' links' 
                sleep(8)
                
        browser.close()
    
    if get_result:
            
    # 6- Analytics:  First check for consistency
            
        completeDict = dict(filter(lambda x,: len(x[1]) == 6, jobDict.items()))   
        
        finalDict = dict(map(lambda (x,y): (x, y[0:5] + [skills_info([y[0]]+y[5])]), completeDict.items()))
        
         
        # Calculate top locations  
    
        location_dict = Counter()
        location_dict.update([finalDict[item][3] for item in finalDict.keys()])    
        location_frame = pd.DataFrame(location_dict.items(), columns = ['Term', 'NumPostings'])
        
        # Calculate top companies - (company, rating) , Num posting
        
        company_dict = Counter()
        company_dict.update([(finalDict[item][2],finalDict[item][1]) for item in finalDict.keys()])
        company_frame = pd.DataFrame(company_dict.items(), columns = ['Term', 'NumPostings'])
            
        # Calculate other analytics
        skill_frame, edu_frame, lang_frame = skills_info(completeDict)
        
        
    # 7- Find your match
     
        myCV = ['Data Scientist', 'PhD','French','Python','R','Matlab','Spark','SQL','Physics']
        
        # first parse the CV
        myCV = [item.lower() for item in myCV]
        
        BestMatch = get_match(myCV,finalDict)    
        
        print 'The top 5 companies matching my CV are:' 
        print  BestMatch.head(5)
    
    
    
# 216.230.228.88