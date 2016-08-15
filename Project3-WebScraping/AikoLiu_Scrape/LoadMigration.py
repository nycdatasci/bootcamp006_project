import pandas as pd
import numpy as np
import os, re, sys
# The state2state migration data consists of 2000+ small files of 20+ years of yearly 
# state 2 state migration data (tracked by the tax returns filed to IRS).
# The data is very dirty in that the format of each file is not always the same, the state
#abbreviations are named almost semi-randomly. 
#We put the soft links of these 2000+ files into a single directory.
#Then we load them, mapping to the state abbreviations back to the state names and store into
#pandas data frames.

def LoadMigration():

    postAbbFile = '/Users/ikovsky/r_wd/data/statePostCode.csv'
    postAbb     = pd.read_csv(postAbbFile,sep='\t')
    country     =['United States','ForeignOverseas']
    dataDir     = '/Users/ikovsky/python_data/IRS/TAX-STATS/soi-tax-stats-migration-data/LinkedData'
    
    translate   = {'^oeg':'oregon','^az':'arizona','^DiCo':'Distr','^dico':'Distr','^vrg':'Virginia','^Miso':'Misso',\
    '^miso':'Misso','^wsc':'Wiscon', '^wiso':'Wiscon','^RhIs':'Rhode','^aka':'arkan','^arkas':'arkan'\
    ,'^Misi':'missi','^nhio':'ohio','^nrbt':'nebra'}

    fileNameList = os.listdir(dataDir)
    p = re.compile('\d+')
    q = re.compile('^[0-9]{4}')
    key = None
    stateDict = StateDict(postAbb)
    data = {}
    oddFormat = False
    
    for name in fileNameList:

             if not re.search('.xls$',name): continue

             if q.search(name): oddFormat = True
             else:  oddFormat = False
             nameTokens = p.split(name)
             token  = "^"+nameTokens[0]
             
             failed = True
             key    = None
 
             for i, state in enumerate(postAbb.iloc[:,0]):

                    if oddFormat: 
                                 break 
                    
                    result = re.search(token,state,flags=re.I)
                    if result is not None: key = state 
                    if result is None: 
                              state2 = re.sub(' ','',state) 
                              result = re.search(token,state2,flags=re.I)
                              key    = state
                    if result is None:
                              state2 = re.sub('West ','W',state) 
                              result = re.search(token,state2,flags=re.I)
                              key    = state
                    if result is None:
                              state2 = re.sub('West ','We',state) 
                              result = re.search(token,state2,flags=re.I)
                              key    = state
                    if result is None:
                              state2 = re.sub('South ','S',state) 
                              result = re.search(token,state2,flags=re.I)
                              key    = state
                    if result is None:
                              state2 = re.sub('South ','So',state) 
                              result = re.search(token,state2,flags=re.I)
                              key    = state
                    if result is None:
                              state2 = re.sub('North ','no',state) 
                              result = re.search(token,state2,flags=re.I)
                              key    = state
                    if result is None:
                              state2 = re.sub('North ','n',state) 
                              result = re.search(token,state2,flags=re.I)
                              key    = state
                    if result is None:
                              state2 = re.sub('New ','ne',state) 
                              result = re.search(token,state2,flags=re.I)
                              key    = state
                    if result is None and translate.get(token) is not None:
                              result = re.search(translate[token],state,flags=re.I) 
                              key    = state
                    if result is not None: break
                    

             if not oddFormat and result is None: 
                         result = re.search(token,country[0],flags=re.I)
                         key    = country[0] 
             if not oddFormat and result is None: 
                         result = re.search(token,country[1],flags=re.I)
                         key    = country[1] 

             if not oddFormat and result: 
                       failed = False

             
             if not oddFormat and failed:
                      print(nameTokens+[name])
                      print(len(nameTokens[0]))

             name1 = re.sub("^"+nameTokens[0],'',name)
             inOrout = '' if oddFormat else InOrOut(name) 
             year  = re.sub(nameTokens[-1]+"$",'',name1)
             year  = name[2:4] if oddFormat else MapYear(year) 
             
             if oddFormat: key = HandleOddFormatName(name,stateDict) 
             data[key+inOrout+":"+year] = LoadFileToDF(dataDir, name)

    return(data)

def LoadFileToDF(dirName, fileName):

    DF = pd.read_excel(dirName+"/"+fileName)
    if DF.shape[1] not in [6,7,8]: raise ValueError("bad column size")
    DF[DF==''] = np.NaN
    naCols = DF.isnull().all(axis=0)
    DF     = DF.loc[:,~naCols]
    redCol  = [t for t in DF.columns if not DF[t].str.contains('PERCENT',case=False).any()] 
    DF      = DF[redCol]
    columns = DF.columns
    gross   = [idx for idx, t in enumerate(DF.columns) if \
    (DF[t].str.contains('INCOME',case=False)[:8].any() and DF[t].str.contains('AGGREGATE',case=False)[:8].any())\
    or (DF[t].str.contains('INCOME',case=False)[:8].any() and DF[t].str.contains('MONEY',case=False)[:8].any())]
    if len(gross)>0:
              idx=gross[-1]
              if idx==0: raise ValueError("") 
              DF = DF.iloc[:,idx-3:idx+1]
              DF.columns = ['Name','Num Returns','Num Exempt','AAGI']
    else: 
              DF = DF.iloc[:,-3:]
              DF.columns = ['Name','Num Returns','Num Exempt']
    missingRow = DF.isnull().any(axis=1)
    DF         = DF.loc[~missingRow,:]
    
    return(DF.apply(pd.to_numeric,errors='ignore'))

def InOrOut(name):

    token = name[-7:-4].lower()
    token = re.sub('[0-9]+','',token)
    if re.search('i|n',token): return(':IN')
    elif re.search('o',token): return(':OUT')
    raise ValueError(name+' makes me confused') 

def HandleOddFormatName(name, stateDict):

    name1 = name[-9:-4]
    stateAbb  = name1[-2:].upper()
    inOrout   = name1[:3].upper()
    if (inOrout == 'GIN'): inOrout = 'IN'
    stateName = stateDict[stateAbb]
    return(stateName+":"+inOrout)

def StateDict(postAbb):

    stateNames = postAbb.iloc[:,0]
    stateAbbs  = postAbb.iloc[:,1]

    ans = dict()
    for idx, abb in enumerate(stateAbbs):
        ans[abb] = stateNames[idx] 
     
    return(ans)


def MapYear(year):

             if len(year) == 2:
                           pass
             elif len(year) == 4:
                           year = year[2:4]
             if len(year) not in [2,4]: 
                           #print(year)
                           #print(name)
                           #print(nameTokens)
                           #956 -> 95-96
                           #8   -> 98 
                           if year == '8': year = '98'
                           elif year == '956': year = '96'
                           else: print(year)

             return(year)
