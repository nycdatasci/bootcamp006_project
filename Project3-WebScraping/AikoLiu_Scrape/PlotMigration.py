import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import numpy.random as random

# Plot the descending sorted SVD eigenvalues 
def PlotEigenValues(eigenValues,key='avg',logOdds=True):

    token = ' Log Odd ' if logOdds else 'Original'
    numStates = eigenValues[key].shape[0]
    plt.plot(np.arange(numStates),eigenValues[key])
    plt.xlabel("Descending Sorted Order")
    plt.ylabel("EigenValues of %s Migration Prob. SVD" %(token))

# Plot the SVD descending sorted eigenvalues from a random matrix 
def PlotRandomSVD(nrow=51,ncol=51,std=1,dist='normal'):

    if dist=='normal':
       Q = random.normal(0,std,nrow * ncol).reshape(nrow,ncol)
    elif dist=='chisq':
       Q = random.chisquare(std,nrow * ncol).reshape(nrow,ncol)
    else:  raise ValueError(dist + " Unknown dist choice")

    U,S,V = np.linalg.svd(np.matrix(Q),full_matrices=True)

    plt.plot(np.arange(S.shape[0]),S)
  
    plt.xlabel('Descending Sorted Order')
    plt.ylabel('%s Random Eigen Values'%(dist.upper()))

# Plot the average of L1 norm of the matrix entries
def PlotMeanResiduals(residuals):

    N = residuals['avg'].shape[0]
    x = np.arange(N).reshape(1,-1)
    y = np.arange(N).reshape(-1,1)

    years = list(residuals.keys())
    years = [y for y in years if y != 'avg']
    years.sort()
    mL1   = np.zeros(len(years))
    l1    = 0.0   

    for yIdx, key in enumerate(years):

        if key == 'avg': continue
        value  = residuals[key].as_matrix()
        mL1[yIdx] = np.mean(np.abs(value[x!=y]))
        l1       += np.abs(value[x!=y])

    l1/=len(years)
    years = np.array([int(y) for y in years])
    plt.plot(years, mL1)
        
    return(l1.mean()) 

#Plot the collection of the correlations values between the K(K-1) residuals of different years
def PlotTSCorrDist(X,ResFct,Time=21): 

    if X.shape[0] != 0.5*Time*(Time-1): raise ValueError("X size not compatible with Time variable") 

    sns.kdeplot(X)
    plt.xlabel('Cosine values of the residuals from different years')
    plt.ylabel('Distribution Density')
    plt.title('Correlation Value KDE Plot of %d degree of freedom' %(ResFct*(ResFct-1)))
    Q = random.normal(0,1,ResFct*(ResFct-1)*Time).reshape(Time,-1)
    A = np.matrix(Q) * np.matrix(Q).transpose() 
    A = A/np.sqrt(np.diag(A).reshape(1,-1)*np.diag(A).reshape(-1,1))
    x = np.arange(Time).reshape(-1,1)
    y = np.arange(Time).reshape(1,-1)
    sns.kdeplot(np.array(A)[x>y].flatten())

#Plot the ratio of cumulative lambda*2 covered by the truncated SVD
def PlotRSquare(eigenValues,key='avg', logOdds=True):

    eigen = eigenValues.get(key)
    if eigen is None: raise ValueError("Invalid key "+key+"!")
    X = eigen
    z = np.cumsum(X**2)/np.sum(X**2)
    plt.plot(np.arange(z.shape[0]),z)
    token = ' Log Odds ' if logOdds else '' 
    plt.title(key+" Migration Prob. "+token+" Matrix T-SVD R^2 Plot")
    plt.grid(True)
