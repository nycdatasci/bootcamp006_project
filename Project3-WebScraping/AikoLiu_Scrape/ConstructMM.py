import pandas as pd
import numpy  as np
import string

#MM means Migration Probability Matrix
#In this utility function we input the raw migration pandas dataframes as an ordered dict
#Then we normalize by the non-immigrant tax population to get the migration probability matrices
def ConstructMM(dataDict, outputByYear=False):

    postAbbFile = '/Users/ikovsky/r_wd/data/statePostCode.csv'

    postAbb = pd.read_csv(postAbbFile,sep='\t')
    numStates  = postAbb.shape[0]
    stateNames = postAbb.iloc[:,0]

    years      = [str(yyyy) for yyyy in range(1991,2012)]

    

    non_migrants = {}
    yearlyMM = {}

    for year in years:

        yearlyMM[year] = pd.DataFrame(np.zeros((numStates,numStates)))
        yearlyMM[year].columns = postAbb.iloc[:,0]
        yearlyMM[year].index   = postAbb.iloc[:,0]
        non_migrants[year] = pd.DataFrame(np.zeros((numStates,1)),index=postAbb.iloc[:,0])
    
    for key, DF in dataDict.items():

        region, inOrout, yy = key.split(':')
        if inOrout=='OUT': continue
        if sum(stateNames.isin([region]))<1: continue

        if yy[0]=='9': year = '19'+yy
        else:          year = '20'+yy

        DF = DF.set_index(['Name'])

        hasTotal = DF.index.str.contains('Non-Migrant',case=False)
        rowIdxes = np.arange(DF.shape[0])[hasTotal]

        if len(rowIdxes)<1:raise ValueError("no non-imigrant row")

        non_migrants[year].ix[region,0] = DF.iloc[rowIdxes[0],0]





    for key, DF in dataDict.items():

        region, inOrout, yy = key.split(':')

        if inOrout=='IN': continue
        if sum(stateNames.isin([region]))<1: 
            continue


        if yy[0]=='9': year = '19'+yy
        else:          year = '20'+yy

        DF = DF.set_index(['Name'])
        
        #hasTotal = DF.index.str.contains('Non-Migrant',case=False)
        #rowIdxes = np.arange(DF.shape[0])[hasTotal]

        #if len(rowIdxes)<1:raise ValueError("no non-imigrant row")

        for state in stateNames:
 
            if state==region:continue
            stateLoc = DF.index.str.contains(state,case=False)
            if sum(stateLoc)<1: continue
            value = DF[stateLoc].ix[0,0]
            if isinstance(value,str): continue
            meanPopulation = 0.5 * (non_migrants[year].ix[region,0] + non_migrants[year].ix[state,0]) 
            yearlyMM[year].ix[region, state] = value*1.0/meanPopulation*10000
           


    return yearlyMM 
