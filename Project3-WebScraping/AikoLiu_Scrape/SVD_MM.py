import pandas as pd
import numpy  as np
import os

#The main function to process a dictionary of Migration probability Matrices
#and return the truncated SVD, the singular values, log-odd domain residuals, the residuals
#in the sigmoid transformed (original) domain.
# numFct controls the cut off. 
def SVD_MM(MM,logOdds=True,numFct=4):

    MM2,states    = ReturnAvg(MM) 

    if logOdds:   MM_logOdds = GetLogOdds(MM2)      

 
    ans,singularValues,residuals,residuals2 = \
    GetTSVD(MM2 if not logOdds else MM_logOdds,logOdds=logOdds,truncated=numFct)
    ans       = DressUpToDF(ans,states)
    residuals = DressUpToDF(residuals,states)
    return(ans,singularValues,residuals,residuals2)

#Save the end result, pandas data frames, of the TSVD to csv files
def SaveDF2CSV(ans,dirName='/Users/ikovsky/r_wd/data/MigrationData/'):

    if not os.path.isdir(dirName): raise IOError(dirName+" not a valid directory name")
    for key, value in ans.items():
        
        fileName = dirName+"/"+key + ".csv"
        value.to_csv(fileName)
    
# Compute the average MM of the whole 1991-2011 period
def ReturnAvg(MM):

    MM2 = {}
    avg = 0
    colNames = []
    for key, value in MM.items():
        avg += value.as_matrix()
        MM2[key] = value.as_matrix()
        if len(colNames)==0: colNames = value.columns

    MM2['avg'] = avg/len(MM.keys())
    return(MM2,colNames) 


# Perform the log odd transformation, Notice that 
# our original MMs are scaled up by 1e4=10000 because the percentages of
# people migrating are very tiny.
def GetLogOdds(MM2):

    ans = {}
    x   = np.arange(MM2['avg'].shape[0]).reshape(1,-1)
    y   = x.reshape((-1,1))
    dia = x==y

    for key, value in MM2.items():

        T = np.log(1e-4*value/(1-1e-4*value)) # we had scale up the transfer matrix by 1e4 for the ease of 
        # visualization, we need to scale it back.
        T[dia] = 0.0
        T[np.isinf(T)] = np.log(1e-5) 
        ans[key] = T

    return(ans)

# Get the truncated SVD
def GetTSVD(MM,truncated=None,logOdds=True):

    ans         = {}
    residuals   = {}
    residuals2  = {}
    singularValues = {}

    x   = np.arange(MM['avg'].shape[0]).reshape(1,-1)
    y   = x.reshape((-1,1))
    dia = x==y

    for key, value in MM.items():

        U,S,V=np.linalg.svd(value,full_matrices=True)
        singularValues[key] = S.copy()
        S1               = S.copy()
        S2               = S.copy()
        S1[truncated:]   = 0
        S2[:truncated]   = 0
        Lambda1          = np.diag(S1)
        A = np.matrix(U)*np.matrix(Lambda1)*np.matrix(V) 
        Lambda2          = np.diag(S2)
        B                = np.matrix(U)*np.matrix(Lambda2)*np.matrix(V) 
        B[dia]           = 0
        residuals[key]   = B
 
        if logOdds:
           A = 1e4/(1+np.exp(-A))
        A[dia]   = 0
        ans[key] = A
        rA       = 1e4/(1+np.exp(-value))-A
        rA[dia]  = 0
        residuals2[key]  = pd.DataFrame(rA) 

    return(ans,singularValues,residuals,residuals2)

# In the SVD computation, we need to convert the Data Frames into numpy matrices
# For display or for storage into csv, we need to convert them back to DataFrame
# This is the funtionality of this function
def DressUpToDF(MMDict,states):

    ans = {}

    for key, value in MMDict.items():

        ans[key] = pd.DataFrame(value,index=states,columns=states)

    return(ans)

# Given a dictionary of the numpy matrices of the same size, extract the
# off diagonals and collect them into a 1D numpy array
def PickUpOffDiag(MMDict):

    K = MMDict['avg'].shape[0]
    x = np.arange(K).reshape(-1,1)
    y = np.arange(K).reshape(1,-1)
    offDiag = x != y
    collection = []
    

    for key, value in MMDict.items():
             if key == 'avg': continue 
             collection.extend(value.as_matrix()[offDiag].flatten())

    return(np.array(collection)) 

# Check if there is evidence against the IID assumption
def CheckIID(MMDict,numFct=4,Time=False,cov=False):


    years = list(MMDict.keys())
    years.sort()
    K = MMDict['avg'].shape[0]-numFct
    U,S,V=np.linalg.svd(MMDict['avg'].as_matrix(),full_matrices=True)

    x = np.arange(K).reshape(-1,1)
    y = np.arange(K).reshape(1,-1)
    offDiag = x != y

    u = np.arange(K*(K-1)).reshape(-1,1)
    v = np.arange(K*(K-1)).reshape(1,-1)
    upperDiag1 = u > v

    T = len(years)-1
    a = np.arange(T).reshape(-1,1)
    b = np.arange(T).reshape(1,-1)
    upperDiag2 = a > b


    resMatrix = np.zeros((np.sum(offDiag),T))


    for idx, key in enumerate(years):

        if key == 'avg': continue 
        value = MMDict[key].as_matrix()
        R     = np.matrix(U).transpose()*value*np.matrix(V).transpose()
        R     = R[numFct:,numFct:]
        resMatrix[:,idx] = np.array(R[offDiag]).flatten()

    if Time is False:
       X = (np.matrix(resMatrix) * np.matrix(resMatrix).transpose())
       if not cov:
          X = np.array(X)/np.sqrt(np.diag(X).reshape(1,-1)*np.diag(X).reshape(-1,1))
       else: X = np.array(X)/T
       X = X[upperDiag1]
    else:
       #Z = np.matrix(resMatrix-np.mean(resMatrix,axis=0).reshape(1,-1))
       Z = np.matrix(resMatrix)
       X = np.array(Z.transpose() * Z)
       if not cov:
          X = np.array(X)/np.sqrt(np.diag(X).reshape(1,-1)*np.diag(X).reshape(-1,1))
       else: 
          X = np.array(X)/Z.shape[0]
       X = X[upperDiag2]
    return(np.array(X).flatten())      
