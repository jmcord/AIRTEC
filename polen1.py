# -*- coding: utf-8 -*-
"""
Created on Thu Sep 19 11:57:13 2019

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


#train the first model for OLEA

train = pd.read_csv('data_model.csv')

#train = train.dropna(how = 'any')


y = train['OLEA']
train = train.drop(columns = ['Unnamed: 0','OLEA','NO2','O3','PM10','year','nombre_year','id'])

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

model_max = lightgbm.train(parameters,
                       train_set = train_pool,
                       valid_sets=test_pool,
                       num_boost_round=5000,
                       early_stopping_rounds=500)

preds = model_max.predict(X_test)

plt.plot(preds, y_test)
r2_score(preds, y_test)

lightgbm.plot_importance(model_max, importance_type='split', max_num_features=20)

preds = pd.Series(preds)
preds.to_csv("lgb_preds.csv")

y_test = pd.Series(y_test)
y_test.to_csv("y_test.csv")

total_preds = model_max.predict(train)
total_preds = pd.Series(total_preds)
total_preds.to_csv("total_preds.csv")


#Try removing AQ 

train = pd.read_csv('data_model.csv')

y = train['OLEA']
train = train.drop(columns = ['Unnamed: 0','OLEA','NO2','O3'])

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

model_max = lightgbm.train(parameters,
                       train_set = train_pool,
                       valid_sets=test_pool,
                       num_boost_round=5000,
                       early_stopping_rounds=500)

preds = model_max.predict(X_test)

plt.plot(preds, y_test)
r2_score(preds, y_test)

lightgbm.plot_importance(model_max, importance_type='split', max_num_features=20)

preds = pd.Series(preds)
preds.to_csv("lgb_preds.csv")

y_test = pd.Series(y_test)
y_test.to_csv("y_test.csv")

total_preds = model_max.predict(train)
total_preds = pd.Series(total_preds)
total_preds.to_csv("total_preds.csv")


























#The same for OLEA with Sf


train = pd.read_csv('data_par_model.csv')

#train = train.dropna(how = 'any')


y = train['OLEA']
train = train.drop(columns = ['Unnamed: 0','OLEA','NO2','O3','PM10','preds1','preds2','sf_raw'])

cols = list(train.columns.values)

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
total_preds.to_csv("total_preds.csv")

#Compute AIC (Akaike Information Criterion)


from math import log
from sklearn.metrics import mean_squared_error
from sklearn.metrics import mean_absolute_error

def calculate_aic(n, mae, num_params):
	aic = n * log(mae) + 2 * num_params
	return aic


mse = mean_squared_error(preds, y_test)
mae = mean_absolute_error(preds, y_test)


aic = calculate_aic(len(y_test), mae, num_params = 21)
print('AIC: %.3f' % aic)
#External validation of this model

#PARA 2018

ara = pd.read_csv('data_model_ara.csv')
y = ara['OLEA']
ara = ara.drop(columns = ['Unnamed: 0','OLEA','NO2','O3','PM10','preds1','preds2','SPI','year'])

ara = ara.reindex(columns = cols)

ara['preds'] = model.predict(ara)

ara_max = max(ara['preds'])
ara_max_index = ara.loc[ara['preds'] == max(ara['preds']),:].yearday
ara_start_index = ara.loc[ara['preds'] > 20,:].yearday

ara['OLEA'] = y

ara_max_real = max(ara['OLEA'])
ara_max_index_real = ara.loc[ara['OLEA'] == max(ara['OLEA']),:].yearday
ara_start_index_real = ara.loc[ara['OLEA'] > 20,:].yearday

#BAR 2018

bar = pd.read_csv('data_model_bar.csv')
y = bar['OLEA']
bar = bar.drop(columns = ['Unnamed: 0','OLEA','NO2','O3','PM10','preds1','preds2','SPI','year'])

bar = bar.reindex(columns = cols)

bar['preds'] = model.predict(bar)

bar_max = max(bar['preds'])
bar_max_index = bar.loc[bar['preds'] == max(bar['preds']),:].yearday
bar_start_index = bar.loc[bar['preds'] > 20,:].yearday

bar['OLEA'] = y

bar_max_real = max(bar['OLEA'])
bar_max_index_real = bar.loc[bar['OLEA'] == max(bar['OLEA']),:].yearday
bar_start_index_real = bar.loc[bar['OLEA'] > 20,:].yearday


#ALCO 2018

alco = pd.read_csv('data_model_alco_TS.csv')
y = alco['OLEA']
alco = alco.drop(columns = ['Unnamed: 0','OLEA','preds1','preds2','SPI','year'])

alco = alco.reindex(columns = cols)

alco['preds'] = model.predict(alco)

alco_max = max(alco['preds'])
alco_max_index = alco.loc[alco['preds'] == max(alco['preds']),:].yearday
alco_start_index = alco.loc[alco['preds'] > 20,:].yearday

alco['OLEA'] = y

alco_max_real = max(alco['OLEA'])
alco_max_index_real = alco.loc[alco['OLEA'] == max(alco['OLEA']),:].yearday
alco_start_index_real = alco.loc[alco['OLEA'] > 20,:].yearday


#VIL 2018

vil = pd.read_csv('data_model_vil_TS.csv')
y = vil['OLEA']
vil = vil.drop(columns = ['Unnamed: 0','OLEA','preds1','preds2','SPI','year'])

vil = vil.reindex(columns = cols)

vil['preds'] = model.predict(vil)

vil_max = max(vil['preds'])
vil_max_index = vil.loc[vil['preds'] == max(vil['preds']),:].yearday
vil_start_index = vil.loc[vil['preds'] > 20,:].yearday

vil['OLEA'] = y

vil_max_real = max(vil['OLEA'])
vil_max_index_real = vil.loc[vil['OLEA'] == max(vil['OLEA']),:].yearday
vil_start_index_real = vil.loc[vil['OLEA'] > 20,:].yearday


#ALC 2018

alc = pd.read_csv('data_model_alc_TS.csv')
y = alc['OLEA']
alc = alc.drop(columns = ['Unnamed: 0','OLEA','preds1','preds2','SPI','year'])

alc = alc.reindex(columns = cols)

alc['preds'] = model.predict(alc)

alc_max = max(alc['preds'])
alc_max_index = alc.loc[alc['preds'] == max(alc['preds']),:].yearday
alc_start_index = alc.loc[alc['preds'] > 20,:].yearday

alc['OLEA'] = y

alc_max_real = max(alc['OLEA'])
alc_max_index_real = alc.loc[alc['OLEA'] == max(alc['OLEA']),:].yearday
alc_start_index_real = alc.loc[alc['OLEA'] > 20,:].yearday

#CIU 2018

ciu = pd.read_csv('data_model_ciu_TS.csv')
y = ciu['OLEA']
ciu = ciu.drop(columns = ['Unnamed: 0','OLEA','preds1','preds2','SPI','year'])

ciu = ciu.reindex(columns = cols)

ciu['preds'] = model.predict(ciu)

ciu_max = max(ciu['preds'])
ciu_max_index = ciu.loc[ciu['preds'] == max(ciu['preds']),:].yearday
ciu_start_index = ciu.loc[ciu['preds'] > 20,:].yearday

ciu['OLEA'] = y

ciu_max_real = max(ciu['OLEA'])
ciu_max_index_real = ciu.loc[ciu['OLEA'] == max(ciu['OLEA']),:].yearday
ciu_start_index_real = ciu.loc[ciu['OLEA'] > 20,:].yearday

#GET 2018

get = pd.read_csv('data_model_get_TS.csv')
y = get['OLEA']
get = get.drop(columns = ['Unnamed: 0','OLEA','preds1','preds2','SPI','year'])

get = get.reindex(columns = cols)

get['preds'] = model.predict(get)

get_max = max(get['preds'])
get_max_index = get.loc[get['preds'] == max(get['preds']),:].yearday
get_start_index = get.loc[get['preds'] > 20,:].yearday.iloc[0]

get['OLEA'] = y

get_max_real = max(get['OLEA'])
get_max_index_real = get.loc[get['OLEA'] == max(get['OLEA']),:].yearday
get_start_index_real = get.loc[get['OLEA'] > 20,:].yearday.iloc[0]



#LEG 2018

leg = pd.read_csv('data_model_leg_TS.csv')
y = leg['OLEA']
leg = leg.drop(columns = ['Unnamed: 0','OLEA','preds1','preds2','SPI','year'])

leg = leg.reindex(columns = cols)

leg['preds'] = model.predict(leg)

leg_max = max(leg['preds'])
leg_max_index = leg.loc[leg['preds'] == max(leg['preds']),:].yearday
leg_start_index = leg.loc[leg['preds'] > 20,:].yearday.iloc[0]

leg['OLEA'] = y

leg_max_real = max(leg['OLEA'])
leg_max_index_real = leg.loc[leg['OLEA'] == max(leg['OLEA']),:].yearday
leg_start_index_real = leg.loc[leg['OLEA'] > 20,:].yearday.iloc[0]



#RET 2018

ret = pd.read_csv('data_model_ret_TS.csv')
y = ret['OLEA']
ret = ret.drop(columns = ['Unnamed: 0','OLEA','preds1','preds2','SPI','year'])

ret = ret.reindex(columns = cols)

ret['preds'] = model.predict(ret)

ret_max = max(ret['preds'])
ret_max_index = ret.loc[ret['preds'] == max(ret['preds']),:].yearday
ret_start_index = ret.loc[ret['preds'] > 20,:].yearday.iloc[0]

ret['OLEA'] = y

ret_max_real = max(ret['OLEA'])
ret_max_index_real = ret.loc[ret['OLEA'] == max(ret['OLEA']),:].yearday
ret_start_index_real = ret.loc[ret['OLEA'] > 20,:].yearday.iloc[0]



#ROZ 2018

roz = pd.read_csv('data_model_roz_TS.csv')
y = roz['OLEA']
roz = roz.drop(columns = ['Unnamed: 0','OLEA','preds1','preds2','SPI','year'])

roz = roz.reindex(columns = cols)

roz['preds'] = model.predict(roz)

roz_max = max(roz['preds'])
roz_max_index = roz.loc[roz['preds'] == max(roz['preds']),:].yearday
roz_start_index = roz.loc[roz['preds'] > 20,:].yearday.iloc[0]

roz['OLEA'] = y

roz_max_real = max(roz['OLEA'])
roz_max_index_real = roz.loc[roz['OLEA'] == max(roz['OLEA']),:].yearday
roz_start_index_real = roz.loc[roz['OLEA'] > 20,:].yearday.iloc[0]


#Predict maxs with model_max



ara_max = pd.read_csv('data_model_ara_max.csv')

ara_max = ara_max[['yearday','PM10','tmin','O3','tmed','tmax','NO2','sol','velmedia','month']]

preds = model_max.predict(ara_max)

max(preds)














#The same for OLEA with previous gam ensembled


train = pd.read_csv('data_par_model.csv')

train = pd.read_csv('data_par_model_spi.csv')

#train = train.dropna(how = 'any')


y = train['OLEA']
train = train.drop(columns = ['Unnamed: 0','OLEA'])

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
total_preds.to_csv("total_preds.csv")



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

train_ara = pd.read_csv('data_model_ara.csv')

y = train_ara['OLEA']

train = train[['NO2','O3','PM10','tmed','tmin','tmax','velmedia','sol','yearday','sf','start_MPS','preds1','preds2','peak']]

train_ara = train_ara[['month','NO2','O3','PM10','tmed','tmin','tmax','dir','velmedia','sol','yearday','id_alc','id_alco','id_ara',
               'id_bar','id_ciu','id_get','id_leg','id_ret','id_roz','id_vil','sf','SPI','start_MPS','preds1','preds2','peak']]

#train = train.dropna(how = 'any')


#train = train.drop(columns = ['Unnamed: 0','OLEA','year'])


preds = model.predict(train_ara)


plt.plot(preds, y)
r2_score(preds, y)

preds = pd.Series(preds)
preds.to_csv("lgb_preds.csv")

y = pd.Series(preds)
y.to_csv("y.csv")

total_preds = model.predict(train)
total_preds = pd.Series(total_preds)
total_preds.to_csv("total_preds.csv")



#BAR 2018



train_bar = pd.read_csv('data_model_bar.csv')

y = train_bar['OLEA']

train = train[['NO2','O3','PM10','tmed','tmin','tmax','velmedia','sol','yearday','sf','start_MPS','preds1','preds2','peak']]


train_bar = train_bar[['month','NO2','O3','PM10','tmed','tmin','tmax','dir','velmedia','sol','yearday','id_alc','id_alco','id_ara',
               'id_bar','id_ciu','id_get','id_leg','id_ret','id_roz','id_vil','sf','SPI','start_MPS','preds1','preds2','peak']]

#train = train.dropna(how = 'any')


#train = train.drop(columns = ['Unnamed: 0','OLEA','year'])


preds = model.predict(train_bar)


plt.plot(preds, y)
r2_score(preds, y)

preds = pd.Series(preds)
preds.to_csv("lgb_preds.csv")

y = pd.Series(preds)
y.to_csv("y.csv")

total_preds = model.predict(train)
total_preds = pd.Series(total_preds)
total_preds.to_csv("total_preds.csv")
















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








