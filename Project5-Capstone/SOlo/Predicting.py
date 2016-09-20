# -*- coding: utf-8 -*-
"""
Created on Sun Sep 18 19:44:33 2016

@author: venkatesh
"""


train_df = pd.read_csv("data_road.csv", index_col=False)

print '0 label count:', train_df[train_df.label == 0]['label'].count()
print '1 label count:', train_df[train_df.label == 1]['label'].count()

y_train = train_df.pop('label').values
X_train = train_df.sort(axis=1).values

# Scaling the data
encoder = OneHotEncoder(categorical_features=[0, 2, 6, 10, 15, 17])
mat = encoder.fit_transform(X_train)
scaler = StandardScaler()
mat2 = scaler.fit_transform(mat.toarray())

# Logistic Regression and Cross Validation
logit = LogisticRegression(class_weight='auto')
cvs = cross_val_score(logit, mat2, y_train, scoring='roc_auc', cv=5)


y_test = test_df.pop('label').values
X_test = test_df.sort(axis=1).values


encoder = OneHotEncoder(categorical_features=[0, 2, 6, 10, 15, 17])
mat_train = encoder.fit_transform(X)
scaler = StandardScaler()
mat_train2 = scaler.fit_transform(mat_train.toarray())
logit = LogisticRegression(class_weight='auto').fit(mat_train2, y_train)
mat_test = encoder.transform(X_test)
mat_test2 = scaler.transform(mat_test.toarray())


predict = logit.predict(mat_test2)
roc_auc_score(predict, y_test)


[row[1] for row in logit.predict_proba(mat_test2)]

# Random Forest and Cross Validation
rnf = RandomForestClassifier(class_weight='auto', n_estimators=100)

cvs1 = cross_val_score(rnf, X_train, y_train, scoring='roc_auc', cv=5)
print cvs1
print np.mean(cvs1)

rnf.fit(X_train, y_train)
roc_auc_score(rnf.predict(X_test), y_test)

#Gradient Boosting
gbc = GradientBoostingClassifier()
cvs3 = cross_val_score(gbc, X, y, scoring='roc_auc', cv=10)
print cvs3
print np.mean(cvs3)

#Support Vector Classification and Cross Validation
svc = SVC(class_weight='auto')

cvs4 = cross_val_score(svc, mat, y, scoring='roc_auc', cv=10)
print cvs4
print np.mean(cvs4)

# Grid param modified to only store the ones preferred
random_forest_grid = {
                      'max_depth': [3, 6, 9],
                      'max_features': ['sqrt', 'log2', None],
                      'min_samples_split': [5, 10, 15],
                      'min_samples_leaf': [5, 10, 15],
                      'bootstrap': [False, True],
                      'n_estimators': [100],
                      'random_state': [None, 20, 200],
                      'class_weight': ['auto']
                     }

rf_gridsearch = GridSearchCV(RandomForestClassifier(),
                             random_forest_grid,
                             n_jobs=-1,
                             verbose=True,
                             scoring='roc_auc')
rf_gridsearch.fit(X, y)

print "best parameters:", rf_gridsearch.best_params_

best_rf_model = rf_gridsearch.best_estimator_

rnf = RandomForestClassifier(class_weight = 'auto')
rnf.fit(X_train, y_train)
rnf_df = pd.DataFrame(X_test, columns=df2.columns)
rnf_df['proba'] = pd.Series([x[1] for x in rnf.predict_proba(X_test)])
rnf_df['label'] = pd.Series(y_test)

rnf_df['proba_bins'] = pd.cut(rnf_df['proba'], bins=3)

roc_auc_score(rnf.predict(X_test), y_test)
