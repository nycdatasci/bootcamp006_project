# ------------------------------------------------------------------------------
# Created by: Venkatesh Subramanian of Team Demibots
# Created in: Aug 2016
# Purpose to: Post-Competition Kaggle Higgs Boson ML Challenge
# ------------------------------------------------------------------------------

from sknn.mlp import Classifier, Layer
from sklearn.cross_validation import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import MinMaxScaler
import pandas as pd
import numpy as np
import pandas as pd
import numpy as np
import math

M = pd.read_csv("training.csv")
M['label1'] = M['Label'].replace(to_replace=['s', 'b'], value=[1, 0])
M = M.drop('Label', 1)

X = M.iloc[:,1:30].values
y = M.iloc[:,32].values
w = M.iloc[:,31].values

X_train, X_test, y_train, y_test, w_train, w_test = train_test_split(X, y, w, test_size=0.33, random_state=42)

print X_test
# w = X_test.iloc[:, features+1].values
#w = X_test[:,32]
print y_train
print w

#nn = Classifier(layers=[Layer("Rectifier", units=100),Layer("Softmax")],learning_rate=0.002,n_iter=30)

pipeline = Pipeline([
        ('min/max scaler', MinMaxScaler(feature_range=(0.0, 1.0))),
        ('neural network',Classifier(layers=[Layer("Sigmoid", units=100),Layer("Softmax")],learning_rate=0.002,n_iter=30))])
nn.fit(X_train, y_train)

#N = pd.read_csv("test.csv")
#N['label1'] = N['Label'].replace(to_replace=['s', 'b'], value=[1, 0])
#predict_proba(X_train, collapse=True)
#y_test = y_test.iloc.values


#y_valid = nn.predict(X_test)

y_prob = nn.predict_proba(X_test)[:, 1]

# print y_prob

cutoff = np.percentile(y_prob, 85)
# print cutoff

y_est = y_prob > cutoff
# print y_est

#y_valid1 = y_valid.T
#


def ams(s, b):
    """
    Approximate Median Significant function to evaluate solutions.
    """
    br = 10.0
    radicand = 2 * ((s + b + br) * math.log(1.0 + s / (b + br)) - s)
    if radicand < 0:
        print 'Radicand is negative.'
        exit()
    else:
        return math.sqrt(radicand)

def score(y, y_est, w):
    """
    Create weighted signal and background sets and calculate the AMS.
    """
    y_signal = w * (y == 1.0)
    y_background = w * (y == 0.0)
    s = np.sum(y_signal * (y_est == 1.0))
    b = np.sum(y_background * (y_est == 1.0))

    return ams(s, b)

score(y_test,y_est,w_test)


def create_submission(test_data, y_test_prob, y_test_est, submit_file):
    """
    Create a new data frame with the submission data.
    """
    temp = pd.DataFrame(y_test_prob, columns=['RankOrder'])
    temp2 = pd.DataFrame(y_test_est, columns=['Class'])
    submit = pd.DataFrame([test_data.EventId, temp.RankOrder, temp2.Class]).transpose()

    # sort it so they're in the ascending order by probability
    submit = submit.sort(['RankOrder'], ascending=True)

    # convert the probabilities to rank order (required by the submission guidelines)
    for i in range(0, y_test_est.shape[0], 1):
        submit.iloc[i, 1] = i + 1

    # re-sort by event ID
    submit = submit.sort(['EventId'], ascending=True)

    # convert the integer classification to (s, b)
    submit['Class'] = submit['Class'].map({1: 's', 0: 'b'})

    # force pandas to treat these columns at int (otherwise will write as floats)
    submit[['EventId', 'RankOrder']] = submit[['EventId', 'RankOrder']].astype(int)

    # finally create the submission file
    submit.to_csv(submit_file, sep=',', index=False, index_label=False)

#score = nn.predict(y_valid,y_test)

submit_file = 'submission.csv'
data_dir = './'

test_data = pd.read_csv("test.csv")
X_test1 = test_data.iloc[:, 1:30].values

y_test_prob = nn.predict_proba(X_test1)[:, 1]

cutoff = np.percentile(y_test_prob, 85)
print cutoff

y_test_est = y_test_prob > cutoff

#create_submission(test_data, y_test_prob, y_test_est, data_dir + submit_file)
