# -*- coding: utf-8 -*-
"""
Created on Tue Nov 12 13:16:10 2019

@author: Jose María
"""


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score
from sklearn.metrics import mean_squared_error
from sklearn import metrics
import datetime
import lightgbm
from sklearn.preprocessing import OneHotEncoder
import keras





#The same for OLEA with Sf

#Choose between 1 and 3 day lags or 3 and 5

train = pd.read_csv('data_par_model_TS.csv')

train = pd.read_csv('data_par_model_TS_3_5.csv')

#train = train.dropna(how = 'any')


y = train['OLEA']
train = train.drop(columns = ['Unnamed: 0','OLEA','peak'])

X_train, X_test, y_train, y_test = train_test_split(train, y, test_size=0.25, random_state=41)

train_pool = lightgbm.Dataset(X_train, 
                  y_train)

test_pool = lightgbm.Dataset(X_test,
                 y_test) 



parameters = {
    'metric': 'rmse',
    'boosting': 'gbdt',
    'num_leaves': 62,
    'feature_fraction': 0.5,
    'bagging_fraction': 0.8,
    'bagging_freq': 40,
    'learning_rate': 0.01,
    'verbose': 0
}

model = lightgbm.train(parameters,
                       train_set = train_pool,
                       valid_sets=test_pool,
                       num_boost_round=5000,
                       early_stopping_rounds=500)

preds = model.predict(X_test)

plt.plot(preds, y_test)
r2_score(preds, y_test)

lightgbm.plot_importance(model, importance_type='split', max_num_features=50)

preds = pd.Series(preds)
preds.to_csv("lgb_preds_TS.csv")

y_test = pd.Series(y_test)
y_test.to_csv("y_test_TS.csv")

total_preds = model.predict(train)
total_preds = pd.Series(total_preds)
total_preds.to_csv("total_preds_TS_3_5.csv")






#K-fold cross-validation

from sklearn.metrics import r2_score
from sklearn.metrics import make_scorer

r2 = make_scorer(r2_score)

cv_results = lightgbm.cv(
        params = parameters,
        train_set = train_pool,
        num_boost_round=100,
        nfold=10,
        metrics = 'r2',
        early_stopping_rounds=10,
        stratified = False
        )

print('\nBest num_boost_round:', len(cv_results['l1-mean']))
print('Best CV score:', cv_results['l1-mean'][-1])

sum(cv_results['rmse-mean'])/len(cv_results['rmse-mean'])
sum(cv_results['rmse-stdv'])/len(cv_results['rmse-stdv'])

 # evaluate using 10-fold cross validation

from sklearn.model_selection import cross_val_score
accuracies = cross_val_score(estimator = model, X = X_train, y = y_train, cv = 10)



#External validation of the ensemble

#ARA 2018

train_ara = pd.read_csv('data_model_ara_TS.csv')

y = train_ara['OLEA']

train_ara = train_ara[['month','tmed','tmin','tmax','dir','velmedia','sol','yearday',
                   'id_alc','id_alco','id_ara','id_bar','id_ciu',
                   'id_get','id_leg','id_ret','id_roz','id_vil','sf','sf_raw','olea_1_day','olea_3_day','start_MPS','high_MPS','preds1','preds2']]

#train = train.dropna(how = 'any')


#train = train.drop(columns = ['Unnamed: 0','OLEA','year'])


preds = model.predict(train_ara)


plt.plot(preds, y)
r2_score(preds, y)

preds = pd.Series(preds)
preds.to_csv("ara_lgbm_TS.csv")




#BAR 2018



train_bar = pd.read_csv('data_model_bar_TS.csv')

y = train_bar['OLEA']

train_bar = train_bar[['month','tmed','tmin','tmax','dir','velmedia','sol','yearday',
                   'id_alc','id_alco','id_ara','id_bar','id_ciu',
                   'id_get','id_leg','id_ret','id_roz','id_vil','sf','sf_raw','olea_1_day','olea_3_day','start_MPS','high_MPS','preds1','preds2']]

#train = train.dropna(how = 'any')


#train = train.drop(columns = ['Unnamed: 0','OLEA','year'])


preds = model.predict(train_bar)


plt.plot(preds, y)
r2_score(preds, y)

preds = pd.Series(preds)
preds.to_csv("bar_lgbm_TS.csv")




#ALCO 2018



train_alco = pd.read_csv('data_model_alco_TS.csv')

y = train_alco['OLEA']

train_alco = train_alco[['month','tmed','tmin','tmax','dir','velmedia','sol','yearday',
                   'id_alc','id_alco','id_ara','id_bar','id_ciu',
                   'id_get','id_leg','id_ret','id_roz','id_vil','sf','sf_raw','olea_1_day','olea_3_day','start_MPS','high_MPS','preds1','preds2']]

#train = train.dropna(how = 'any')


#train = train.drop(columns = ['Unnamed: 0','OLEA','year'])


preds = model.predict(train_alco)


plt.plot(preds, y)
r2_score(preds, y)

preds = pd.Series(preds)
preds.to_csv("alco_lgbm_TS.csv")




#VIL 2018



train_vil = pd.read_csv('data_model_vil_TS.csv')

y = train_vil['OLEA']

train_vil = train_vil[['month','tmed','tmin','tmax','dir','velmedia','sol','yearday',
                   'id_alc','id_alco','id_ara','id_bar','id_ciu',
                   'id_get','id_leg','id_ret','id_roz','id_vil','sf','sf_raw','olea_1_day','olea_3_day','start_MPS','high_MPS','preds1','preds2']]

#train = train.dropna(how = 'any')


#train = train.drop(columns = ['Unnamed: 0','OLEA','year'])


preds = model.predict(train_vil)


plt.plot(preds, y)
r2_score(preds, y)

preds = pd.Series(preds)
preds.to_csv("vil_lgbm_TS.csv")




#ALC 2018



train_alc = pd.read_csv('data_model_alc_TS.csv')

y = train_alc['OLEA']

train_alc = train_alc[['month','tmed','tmin','tmax','dir','velmedia','sol','yearday',
                   'id_alc','id_alco','id_ara','id_bar','id_ciu',
                   'id_get','id_leg','id_ret','id_roz','id_vil','sf','sf_raw','olea_1_day','olea_3_day','start_MPS','high_MPS','preds1','preds2']]

#train = train.dropna(how = 'any')


#train = train.drop(columns = ['Unnamed: 0','OLEA','year'])


preds = model.predict(train_alc)


plt.plot(preds, y)
r2_score(preds, y)

preds = pd.Series(preds)
preds.to_csv("alc_lgbm_TS.csv")





#CIU 2018



train_ciu = pd.read_csv('data_model_ciu_TS.csv')

y = train_ciu['OLEA']

train_ciu = train_ciu[['month','tmed','tmin','tmax','dir','velmedia','sol','yearday',
                   'id_alc','id_alco','id_ara','id_bar','id_ciu',
                   'id_get','id_leg','id_ret','id_roz','id_vil','sf','sf_raw','olea_1_day','olea_3_day','start_MPS','high_MPS','preds1','preds2']]

#train = train.dropna(how = 'any')


#train = train.drop(columns = ['Unnamed: 0','OLEA','year'])


preds = model.predict(train_ciu)


plt.plot(preds, y)
r2_score(preds, y)

preds = pd.Series(preds)
preds.to_csv("ciu_lgbm_TS.csv")




#GET 2018



train_get = pd.read_csv('data_model_get_TS.csv')

y = train_get['OLEA']

train_get = train_get[['month','tmed','tmin','tmax','dir','velmedia','sol','yearday',
                   'id_alc','id_alco','id_ara','id_bar','id_ciu',
                   'id_get','id_leg','id_ret','id_roz','id_vil','sf','sf_raw','olea_1_day','olea_3_day','start_MPS','high_MPS','preds1','preds2']]

#train = train.dropna(how = 'any')


#train = train.drop(columns = ['Unnamed: 0','OLEA','year'])


preds = model.predict(train_get)


plt.plot(preds, y)
r2_score(preds, y)

preds = pd.Series(preds)
preds.to_csv("get_lgbm_TS.csv")





#LEGANÉS 2018



train_leg = pd.read_csv('data_model_leg_TS.csv')

y = train_leg['OLEA']

train_leg = train_leg[['month','tmed','tmin','tmax','dir','velmedia','sol','yearday',
                   'id_alc','id_alco','id_ara','id_bar','id_ciu',
                   'id_get','id_leg','id_ret','id_roz','id_vil','sf','sf_raw','olea_1_day','olea_3_day','start_MPS','high_MPS','preds1','preds2']]

#train = train.dropna(how = 'any')


#train = train.drop(columns = ['Unnamed: 0','OLEA','year'])


preds = model.predict(train_leg)


plt.plot(preds, y)
r2_score(preds, y)

preds = pd.Series(preds)
preds.to_csv("leg_lgbm_TS.csv")





#RETIRO 2018



train_ret = pd.read_csv('data_model_ret_TS.csv')

y = train_ret['OLEA']

train_ret = train_ret[['month','tmed','tmin','tmax','dir','velmedia','sol','yearday',
                   'id_alc','id_alco','id_ara','id_bar','id_ciu',
                   'id_get','id_leg','id_ret','id_roz','id_vil','sf','sf_raw','olea_1_day','olea_3_day','start_MPS','high_MPS','preds1','preds2']]

#train = train.dropna(how = 'any')


#train = train.drop(columns = ['Unnamed: 0','OLEA','year'])


preds = model.predict(train_ret)


plt.plot(preds, y)
r2_score(preds, y)

preds = pd.Series(preds)
preds.to_csv("ret_lgbm_TS.csv")




#ROZAS 2018



train_roz = pd.read_csv('data_model_roz_TS.csv')

y = train_roz['OLEA']

train_roz = train_roz[['month','tmed','tmin','tmax','dir','velmedia','sol','yearday',
                   'id_alc','id_alco','id_ara','id_bar','id_ciu',
                   'id_get','id_leg','id_ret','id_roz','id_vil','sf','sf_raw','olea_1_day','olea_3_day','start_MPS','high_MPS','preds1','preds2']]

#train = train.dropna(how = 'any')


#train = train.drop(columns = ['Unnamed: 0','OLEA','year'])


preds = model.predict(train_roz)


plt.plot(preds, y)
r2_score(preds, y)

preds = pd.Series(preds)
preds.to_csv("roz_lgbm_TS.csv")






























#train the first model for PLAN

train = pd.read_csv('data_model_plat.csv')

#train = train.dropna(how = 'any')


y = train['PLAT']
train = train.drop(columns = ['Unnamed: 0','PLAT'])

X_train, X_test, y_train, y_test = train_test_split(train, y, test_size=0.25, random_state=41)

train_pool = lightgbm.Dataset(X_train, 
                  y_train)

test_pool = lightgbm.Dataset(X_test,
                 y_test) 



parameters = {
    'metric': 'rmse',
    'boosting': 'gbdt',
    'num_leaves': 62,
    'feature_fraction': 0.5,
    'bagging_fraction': 0.8,
    'bagging_freq': 40,
    'learning_rate': 0.01,
    'verbose': 0
}

model = lightgbm.train(parameters,
                       train_set = train_pool,
                       valid_sets=test_pool,
                       num_boost_round=5000,
                       early_stopping_rounds=500)

preds = model.predict(X_test)

plt.plot(preds, y_test)
r2_score(preds, y_test)

lightgbm.plot_importance(model, importance_type='split', max_num_features=20)

preds = pd.Series(preds)
preds.to_csv("lgb_preds_plat.csv")

y_test = pd.Series(y_test)
y_test.to_csv("y_test_plat.csv")

total_preds = model.predict(train)
total_preds = pd.Series(total_preds)
total_preds.to_csv("total_preds_plat.csv")










#Lo mismo con un adelanto de 5 días



#The same for OLEA with previous gam ensembled


train = pd.read_csv('data_par_model_TS_5.csv')

#train = train.dropna(how = 'any')


y = train['olea_5_day']
train = train.drop(columns = ['Unnamed: 0','olea_5_day'])

X_train, X_test, y_train, y_test = train_test_split(train, y, test_size=0.25, random_state=41)

train_pool = lightgbm.Dataset(X_train, 
                  y_train)

test_pool = lightgbm.Dataset(X_test,
                 y_test) 



parameters = {
    'metric': 'rmse',
    'boosting': 'gbdt',
    'num_leaves': 62,
    'feature_fraction': 0.5,
    'bagging_fraction': 0.8,
    'bagging_freq': 40,
    'learning_rate': 0.01,
    'verbose': 0
}

model = lightgbm.train(parameters,
                       train_set = train_pool,
                       valid_sets=test_pool,
                       num_boost_round=5000,
                       early_stopping_rounds=500)

preds = model.predict(X_test)

plt.plot(preds, y_test)
r2_score(preds, y_test)

lightgbm.plot_importance(model, importance_type='split', max_num_features=20)

preds = pd.Series(preds)
preds.to_csv("lgb_preds.csv")

y_test = pd.Series(y_test)
y_test.to_csv("y_test.csv")

total_preds = model.predict(train)
total_preds = pd.Series(total_preds)
total_preds.to_csv("total_preds_lightgbm_TS.csv")
