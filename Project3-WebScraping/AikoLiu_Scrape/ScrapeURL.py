from bs4 import BeautifulSoup
from collections import OrderedDict
import urllib
import requests
import re,configparser
import argparse

# The main entry point of this module
def ScrapeURL(cfgFile='./ScrapeURL.cfg'):

    cfg = configparser.ConfigParser()
    cfg.read(cfgFile)
    url = cfg.get('ScrapeURL','url')
    linkObj = LinkObj(cfg, name='TAX-STATS',url=url,ancesters=[])
    linkObj.ScrapeLinks()
    linkObj.FollowLinks()
    return(linkObj)

# The Node Object abstracting a link, storing the related information related to scraping
# The program will follow the instruction from the user and follow the link to scrape/download
# the files at the terminal nodes.
# To avoid scraping too much garbage, we need to design letting the user decide what key words
# of interest to follow, what to avoid
class LinkObj(object):

      def __init__(self,cfg,name,url,ancesters=[],nodes_nickName=''):

          self.cfg       = cfg
          self.nameSpace = ''
          x = str.split(name,'/')
          if len(x)>1: 
             self.name = x[-1]
             self.nameSpace = x[0]
          else: self.name      = name
          if nodes_nickName is not None and nodes_nickName != '':
                   self.nickName = nodes_nickName
          else: self.nickName = self.name
          if not self.cfg.has_section(self.nickName):self.cfg.add_section(self.nickName)
          self.url       = url
          self.prePend   = bool(int(self.cfg.get(self.nickName,'prePend')))
          self.ancesters = ancesters
          self.linkDict = OrderedDict()
          x=str.split(re.sub(' ','',self.cfg.get(self.nickName, 'search')),',')
          self.findAllTokens = str.split(re.sub(' ','',self.cfg.get(self.nickName,'findAllTokens')),',')
          self.findAllTokens = [x for x in self.findAllTokens if x!='']
          self.searchToken = '|'.join(x)
          ignore = str.split(re.sub(' ','',self.cfg.get(self.nickName,'ignore')),',')
          self.ignore      = '|'.join(ignore)
          self.nodes       = []
          self.nodes_nickName = self.cfg.get(self.nickName,'nodes_nickName')
          self.excludeFile = str.split(re.sub(' ','',self.cfg.get(self.nickName, 'excludeFile')),',')
          self.excludeFile = [x for x in self.excludeFile if x!='']
          self.excludeFile = "|".join([t+"$" for t in self.excludeFile])
          self.followNumNodes = int(self.cfg.get(self.nickName,'followNumNodes'))
          self.includeFile = str.split(re.sub(' ','',self.cfg.get(self.nickName, 'includeFile')),',')
          self.includeFile = [x for x in self.includeFile if x!='']
          self.includeFile = "|".join([t+"$" for t in self.includeFile])
          self.rootDir     = self.cfg.get(self.nickName,'rootDir')
          self.terminal    = bool(re.search(self.includeFile,self.url))
          self.logLevel    = int(self.cfg.get(self.nickName,'loglevel'))
          

      def ScrapeLinks(self):

          if self.terminal: 
                    self.DownLoadLink()
                    return
          if self.logLevel == 1: print(self.name+" ScrapeLinks")
          r = urllib.request.urlopen(self.url)
          soup  = BeautifulSoup(r,'lxml')
          A     = soup.find_all('a',href=True)
          self.PrintResultSet(A)
          self.IterFindAll(soup,level=0)

      def FollowLinks(self):

          if self.terminal: return
          followNumNodes = self.followNumNodes if self.followNumNodes>=0 else len(self.nodes)
          print(self.name + " FollowLinks")
          count = 0
          for token, url in self.linkDict.items():
              if count >= self.followNumNodes: break  
              self.nodes.append(LinkObj(self.cfg,name=token,url=url,ancesters=self.ancesters+[self.name],nodes_nickName=self.nodes_nickName))
              self.nodes[-1].ScrapeLinks()
              self.nodes[-1].FollowLinks()
              count+=1 


      def IterFindAll(self, X, level):

          if self.logLevel==1: print(self.name + " IterFindAll")
          depth = len(self.findAllTokens)
          if (depth>=level+1): 
             Y = X.find_all(self.findAllTokens[level])
             for y in Y:
                      self.IterFindAll(y,level+1)
          else:
             Y = X.find_all(['a','b'])
             #Y = X.find_all('a',href=True)
             strong = '' 
             for a in Y:
                   text = a.string
                   if a.name=='b': 
                                     if a.string is not None and re.search('^[A-Za-z]+[\w_\- ]*[\w]',a.string): 
                                           strong = a.string 
                                     #print(strong)
                                     continue
                   elif a.get('href') is not None: link = a['href']
                   else: continue
                   if text is None:  text = str.split(link,'/')[-1]
                   if self.searchToken=='':
                                        if self.logLevel == 1: print(self.name + "no search Token") 
                                        continue 
                   elif (len(self.excludeFile)>0 and re.search(self.excludeFile,link,flags=re.I|re.X)): continue
                   elif re.search(self.searchToken,text,flags=re.I|re.X):
                                        if strong != '' and self.prePend: text = strong + "/" + text
                                        self.linkDict[text] = urllib.parse.urljoin(self.url,link)    


      def PrintResultSet(self,A):

            if self.logLevel > 1: return
            for item in A:
                link = item['href']
                text = str.split(link,'/')[-1] if item.string is None else item.string
                if re.search(self.ignore,text,flags=re.I|re.X): continue
                if re.search(self.ignore,link,flags=re.I|re.X): continue
                print(self.name+"<<"+text+":"+link) 

      def DownLoadLink(self):

          path=self.rootDir + "/" + "/".join(self.ancesters) + "/" + self.nameSpace+"/"+self.name+"/"
          fileName = self.url.split('/')[-1]
          if not re.search(self.searchToken,fileName): return
          print(path+fileName)   
          import os
          if not os.path.exists(path):
                 os.makedirs(path)
          elif os.path.exists(path+fileName): 
                print(path+fileName+" exists!")
                return
          urllib.request.urlretrieve(self.url,path+fileName)  

if __name__ == "__main__":

   parser = argparse.ArgumentParser('use ScrapeURL to scrape and download a tree of links')
   parser.add_argument('-cfg',type=str,help='command line option to control the ScrapeURL by passing a config fileName') 
   args  = parser.parse_args()
   if args.cfg is not None:
           ScrapeURL(cfgFile=args.cfg)                   
