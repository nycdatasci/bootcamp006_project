from selenium import webdriver
from bs4 import BeautifulSoup # For HTML parsing
import urllib2 # Website connections
import re # Regular expressions
from time import sleep # To prevent overwhelming the server between connections
from collections import Counter # Keep track of our term counts
from nltk.corpus import stopwords # Filter out stopwords, such as 'the', 'or', 'and'
import pandas as pd # For converting results to a dataframe and bar chart plots
from selenium.webdriver.common import action_chains, keys
import numpy as np

# 1- Scrape for job posting list in Glassdoor


#def skills_info(city=None, state=None):
#    '''
#    This function will take a desired city/state and look for all new job postings
#    on GlassDoor.com. It will crawl all of the job postings and keep track of how many
#    use a preset list of typical data science skills. The final percentage for each skill
#    is then displayed at the end of the collation.
#
#    Inputs: The location's city and state. These are optional. If no city/state is input,
#    the function will assume a national search (this can take a while!!!).
#    Input the city/state as strings, such as skills_info('Chicago', 'IL').
#    Use a two letter abbreviation for the state.
#
#    Output: A bar chart showing the most commonly desired skills in the job market for
#    a data scientist.
#    '''
#
#    final_job = 'data+scientist'  # searching for data scientist exact fit("data scientist" on Glassdoor search)
#    website = 'https://www.glassdoor.com/Job/jobs.htm?suggestCount=10&suggestChosen=true&clickSource=searchBtn&typedKeyword=Da&sc.keyword=data+scientist&locT=C&locId=1132348&jobType='
#
#    # Make sure the city specified works properly if it has more than one word (such as San Francisco)
#    if city is not None:
#        final_city = city.split()
#        final_city = '+'.join(word for word in final_city)
#        final_site_list = ['http://www.glassdoor.com/jobs?q=%22', final_job, '%22&l=', final_city,
#                           '%2C+', state]  # Join all of our strings together so that indeed will search correctly
#    else:
#        final_site_list = ['http://www.glassdoor.com/jobs?q="', final_job, '"']
#
#    final_site = ''.join(final_site_list)  # Merge the html address together into one string
#
#    base_url = 'http://www.indeed.com'
#
#    try:
#        html = urllib2.urlopen(final_site).read()  # Open up the front page of our search first
#    except:
#        'That city/state combination did not have any jobs. Exiting . . .'  # In case the city is invalid
#        return
#    #soup = BeautifulSoup(html)  # Get the html from the first page
#
#    return
    
###############################################################################
def get_pause():
    return  np.random.choice(range(4,10))
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
        
       
    text = text.lower().split()  # Go to lower case and split them apart
        
        
    stop_words = set(stopwords.words("english")) # Filter out any stop words
    text = [w for w in text if not w in stop_words]
        
        
        
    text = list(set(text)) # Last, just get the set of these. Ignore counts (we are just looking at whether a term existed
                            # or not on the website)
        
    return text 


###############################################################################
    
###############################################################################

# 2- Scrape for text in each link if the id

def init_glassdoor():
    chrome_options = webdriver.ChromeOptions()
    driver = '/Users/Diego/Documents/NYCDSA/Project 3/chromedriver'
    chrome_options.add_argument('--disable-extensions')
    chrome_options.add_argument('--profile-directory=Default')
    chrome_options.add_argument("--incognito")
    chrome_options.add_argument("--disable-plugins-discovery");
    chrome_options.add_argument("--start-maximized")
    browser = webdriver.Chrome(driver, chrome_options=chrome_options)
#    browser = webdriver.Chrome(driver)
    
    sleep(10)
    browser.get('https://www.glassdoor.com/index.htm')
    return browser


###############################################################################
##############################################################################       

if __name__ == "__main__":
    
    
    jobName_lst = ['Data Scientist', 'Data Analyst']
    jobName = np.random.choice(jobName_lst)

    city_lst = ['San Cose','New York','San Francisco','Detroit','Washington','Austin','Boston','Los Angeles',' ']
    city = np.random.choice(city_lst)        
    
    browser = init_glassdoor()
    
#    scraper = searchJobs(jobName, city=None):

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
        
        sleep(10)
        browser.execute_script("document.querySelectorAll('button.gd-btn')[1].click()")
        
        # Set up starting page 
        initial_url = browser.current_url
        pages = 10
        jobDict = {}
        link = []
        
        # Find pages link
        
        link_page = browser.find_elements_by_class_name('page')[1:]
        # Find brief description
        
        for page in link_page:
            
        
            # Extract useful classes
            jobPosting =browser.find_elements_by_class_name('jobListing')
            
            # Create basic job Dictionary - but first check whether the keys exist    
            newPost = filter(lambda b: b[0] not in jobDict.keys(),
                        map(lambda a: (a.get_attribute('data-id'), a), jobPosting))
            
            # then process the tuple and update the dictionary
            jobData =  map(lambda (a,b): (a,b.text.encode("utf8").split('\n')[0:4]),newPost)
            jobDict = dict(jobData)    
            
            # finally find the links: make a tuple with id so that you can use the Dict
            link = map(lambda (c,d): link.append(  \
                   (c,d.find_element_by_partial_link_text(jobDict[c][0])) \
                   ), newPost)
            
            page.click()             
            sleep(get_pause())
                
           
           
        # ... and now scrape the links
            
        for element in link:
            
            # Extract link and click it            
            element[1].click()
            sleep(get_pause())
            
            # Extract text
            desc_list = browser.find_element_by_xpath('//*[@id="BasicInfo"]/div[2]/div').text
            description = text_cleaner(desc_list)
            
            # Update dictionary
            jobDict[element[0]].append(description)
            sleep(get_pause())


    
    browser.quit()