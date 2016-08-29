# ------------------------------------------------------------------------------
# Extended by: Team Demibots (Bernard O, Nanda R)
# Created in: Aug 2016
# Purpose to: Post-Competition Kaggle Higgs Boson ML Challenge
# ------------------------------------------------------------------------------
# Adapted fr: Original framework code sourced from TimR
# Team Demibots extended and enhanced framework to work with more models
# ------------------------------------------------------------------------------

import os, math, time, pickle, sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn import cross_validation
from sklearn import decomposition
from sklearn import ensemble
from sklearn import linear_model
from sklearn import naive_bayes
from sklearn import preprocessing
from sklearn import svm
from sklearn.svm import SVC
from sklearn.externals import joblib
import xgboost as xgb


def ams(s, b):
    # AMS function
    br = 10.0
    radicand = 2 * ((s + b + br) * math.log(1.0 + s / (b + br)) - s)
    if radicand < 0:
        print 'Radicand is negative.'
        exit()
    else:
        return math.sqrt(radicand)


def process_training_data(filename, features, impute, standardize, whiten):
    # Reads in training data and prepares numpy arrays.
    training_data = pd.read_csv(filename, sep=',')

    # add a nominal label (0, 1)
    temp = training_data['Label'].replace(to_replace=['s', 'b'], value=[1, 0])
    training_data['Nominal'] = temp

    X = training_data.iloc[:, 1:features+1].values
    y = training_data.iloc[:, features+3].values
    w = training_data.iloc[:, features+1].values

    # optionally impute the -999 values
    if impute == 'mean':
        imp = preprocessing.Imputer(missing_values=-999, strategy="mean")
        X = imp.fit_transform(X)
    elif impute == "median":
        imp = preprocessing.Imputer(missing_values=-999, strategy="median")
        X = imp.fit_transform(X)
    elif impute == "most_frequent":
        imp = preprocessing.Imputer(missing_values=-999, strategy="most_frequent")
        X = imp.fit_transform(X)
    elif impute == 'zeros':
        X[X == -999] = 0

    # create a standardization transform
    scaler = None
    if standardize:
        scaler = preprocessing.StandardScaler()
        scaler.fit(X)

    # create a PCA transform
    pca = None
    if whiten:
        pca = decomposition.PCA(whiten=True)
        pca.fit(X)

    return training_data, X, y, w, scaler, pca


def train(X, y, w, alg, scaler, pca, features):
    # Trains a new model using the training data.
    if scaler is not None:
        X = scaler.transform(X)

    if pca is not None:
        X = pca.transform(X)

    if alg == 'xgboost':
        # use a separate process for the xgboost library
        return train_xgb(X, y, w, scaler, pca)

    t0 = time.time()

    if alg == 'bayes':
        model = naive_bayes.GaussianNB()
    elif alg == 'logistic':
        model = linear_model.LogisticRegression()
    elif alg == 'svm':
        model = svm.SVC(probability=True)   # kernel = linear, poly, rbf, sigmoid, precomputed
    elif alg == 'boost':
        model = ensemble.GradientBoostingClassifier(n_estimators=100, max_depth=10,
            min_samples_split=200, min_samples_leaf=200, max_features=6)
    elif alg == 'forest':
        model = ensemble.RandomForestClassifier(n_estimators=100, max_depth=10,
            min_samples_split=200, min_samples_leaf=200, max_features=6)
    else:
        print 'No model defined for ' + alg
        exit()

    model.fit(X, y)

    t1 = time.time()
    print 'Model trained in {0:.3f} s.'.format(t1 - t0)

    return model


def train_xgb(X, y, w, scaler, pca):
    # Trains a boosted trees model using the XGBoost library.
    t0 = time.time()

    xgmat = xgb.DMatrix(X, label=y, missing=-999.0, weight=w)

    w_pos = sum(w[i] for i in range(len(y)) if y[i] == 1)
    w_neg = sum(w[i] for i in range(len(y)) if y[i] == 0)

    param = {}
    param['objective'] = 'binary:logitraw'
    param['scale_pos_weight'] = w_neg/w_pos
    param['eta'] = 0.12
    param['max_depth'] = 10
    param['subsample'] = 0.9
    param['eval_metric'] = 'auc'
    param['silent'] = 1

    plst = list(param.items())
    watchlist = []

    model = xgb.train(plst, xgmat, 128, watchlist)

    t1 = time.time()
    print 'Model trained in {0:.3f} s.'.format(t1 - t0)

    return model


def predict(X, model, alg, threshold, scaler, pca):
    # Predicts the probability of a positive outcome and converts the
    # probability to a binary prediction based on the cutoff percentage.
    if scaler is not None:
        X = scaler.transform(X)

    if pca is not None:
        X = pca.transform(X)

    if alg == 'xgboost':
        xgmat = xgb.DMatrix(X, missing=-999.0)
        y_prob = model.predict(xgmat)
    else:
        y_prob = model.predict_proba(X)[:, 1]

    cutoff = np.percentile(y_prob, threshold)
    y_est = y_prob > cutoff

    return y_prob, y_est


def score(y, y_est, w):
    # Create weighted signal and background sets and calculate the AMS.
    y_signal = w * (y == 1.0)
    y_background = w * (y == 0.0)
    s = np.sum(y_signal * (y_est == 1.0))
    b = np.sum(y_background * (y_est == 1.0))

    return ams(s, b)


def cross_validate(X, y, w, alg, scaler, pca, threshold, features):
    # Perform cross-validation on the training set and compute the AMS scores.
    scores = [0,0,0]
    folds = cross_validation.StratifiedKFold(y, n_folds=3)
    i = 0

    for i_train, i_val in folds:
        # create the training and validation sets
        X_train, X_val = X[i_train], X[i_val]
        y_train, y_val = y[i_train], y[i_val]
        w_train, w_val = w[i_train], w[i_val]

        # normalize the weights
        w_train[y_train == 1] *= (sum(w[y == 1]) / sum(w[y_train == 1]))
        w_train[y_train == 0] *= (sum(w[y == 0]) / sum(w_train[y_train == 0]))
        w_val[y_val == 1] *= (sum(w[y == 1]) / sum(w_val[y_val == 1]))
        w_val[y_val == 0] *= (sum(w[y == 0]) / sum(w_val[y_val == 0]))

        # train the model
        model = train(X_train, y_train, w_train, alg, scaler, pca, features)

        # predict and score performance on the validation set
        y_val_prob, y_val_est = predict(X_val, model, alg, threshold, scaler, pca)
        scores[i] = score(y_val, y_val_est, w_val)
        i += 1

    return np.mean(scores)


def process_test_data(filename, features, impute):
    # Reads in test data and prepares numpy arrays.
    test_data = pd.read_csv(filename, sep=',')
    X_test = test_data.iloc[:, 1:features+1].values

    # optionally impute the -999 values
    if impute == 'mean':
        imp = preprocessing.Imputer(missing_values=-999, strategy="mean")
        X = imp.fit_transform(X)
    elif impute == "median":
        imp = preprocessing.Imputer(missing_values=-999, strategy="median")
        X = imp.fit_transform(X)
    elif impute == "most_frequent":
        imp = preprocessing.Imputer(missing_values=-999, strategy="most_frequent")
        X = imp.fit_transform(X)
    elif impute == 'zeros':
        X[X == -999] = 0

    return test_data, X_test


def create_submission(test_data, y_test_prob, y_test_est, submit_file):
    # Create a new data frame with the submission data.
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


def main():
    # perform some initialization
    alg = 'bayes'
    features = 25
    threshold = 80
    impute = 'none'
    standardize = True
    whiten = True
    load_training_data = True
    train_model = True
    create_submission_file = False
    code_dir = './'
    data_dir = './'
    training_file = ''
    test_file = ''
    submit_file = 'submission.csv'

    os.chdir(code_dir)

    print 'Starting process...'
    print 'alg={0}, impute={1}, standardize={2}, whiten={3}, threshold={4}'.format(
        alg, impute, standardize, whiten, threshold)

    if load_training_data:
        print 'Reading in training data...'
        training_data, X, y, w, scaler, pca = process_training_data(
            data_dir + training_file, features, impute, standardize, whiten)

    if train_model:
        print 'Training model on full data set...'
        model = train(X, y, w, alg, scaler, pca, features)

        print 'Calculating predictions...'
        y_prob, y_est = predict(X, model, alg, threshold, scaler, pca)

        print 'Calculating AMS...'
        ams_val = score(y, y_est, w)
        print 'AMS =', ams_val

        print 'Performing cross-validation...'
        val = cross_validate(X, y, w, alg, scaler, pca, threshold, features)
        print'Cross-validation AMS =', val

    if create_submission_file:
        print 'Reading in test data...'
        test_data, X_test = process_test_data(data_dir + test_file, features, impute)

        print 'Predicting test data...'
        y_test_prob, y_test_est = predict(X_test, model, alg, threshold, scaler, pca)

        print 'Creating submission file...'
        create_submission(test_data, y_test_prob, y_test_est, data_dir + submit_file)

    print 'Process complete.'


if __name__ == "__main__":
    main()
