# -*- coding: utf-8 -*-
"""
Created on Wed Jun 26 15:59:59 2019

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



#train the first model for PLAN

train = pd.read_csv('datos_totales.csv')

#train = train.dropna(how = 'any')


y = train['Actinobacteria']
train = train.drop(columns = ['Unnamed: 0'])
#, 'Proteobacteria', 'FECHA', 'AQISD', 'Station_type', 'nombre', 
#                              'provincia', 'horatmin', 'horatmax', 'horaracha', 'horaPresMax', 'horaPresMin', 'Place', 'campaign'])

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






#train the first model for OLEA

train = pd.read_csv('datos.csv')

train = train.dropna(how = 'any')

train['PLAN_pred'] = model_plan.predict(train)

y = train['OLEA']
train = train.drop(columns = ['Unnamed: 0', 'POAC', 'PTOTAL', 'PLAT', 'PLAN', 'CUPR', 'OLEA'])

X_train, X_test, y_train, y_test = train_test_split(train, y, test_size=0.25, random_state=41)

categorical_features = ['month']

train_pool = lightgbm.Dataset(X_train, 
                  y_train, categorical_feature = categorical_features)

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

model_olea = lightgbm.train(parameters,
                       train_set = train_pool,
                       valid_sets=test_pool,
                       num_boost_round=5000,
                       early_stopping_rounds=500)





#train the first model for CUPR

train = pd.read_csv('datos.csv')

train = train.dropna(how = 'any')


y = train['CUPR']
train = train.drop(columns = ['Unnamed: 0', 'POAC', 'PTOTAL', 'PLAT', 'PLAN', 'CUPR', 'OLEA'])

X_train, X_test, y_train, y_test = train_test_split(train, y, test_size=0.25, random_state=41)

categorical_features = ['month']

train_pool = lightgbm.Dataset(X_train, 
                  y_train, categorical_feature = categorical_features)

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

model_cupr = lightgbm.train(parameters,
                       train_set = train_pool,
                       valid_sets=test_pool,
                       num_boost_round=5000,
                       early_stopping_rounds=500)



#train the first model for PTOTAL

train = pd.read_csv('datos.csv')

T = pd.read_csv('T.csv')

train['T'] = T['x']

train = train.dropna(how = 'any')


y = train['PTOTAL']
train = train.drop(columns = ['Unnamed: 0', 'POAC', 'PTOTAL', 'PLAT', 'PLAN', 'CUPR', 'OLEA'])

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

model_ptotal = lightgbm.train(parameters,
                       train_set = train_pool,
                       valid_sets=test_pool,
                       num_boost_round=5000,
                       early_stopping_rounds=500)



# Train the second model
#

train = pd.read_csv('datos.csv')

T = pd.read_csv('T.csv')

train['T'] = T['x']

train = train.dropna(how = 'any')

y = train['POAC']
train = train.drop(columns = ['Unnamed: 0', 'POAC', 'PTOTAL', 'PLAT', 'PLAN', 'CUPR', 'OLEA'])
train['PLAN_pred'] = model_plan.predict(train)
train['OLEA_pred'] = model_olea.predict(train)
train['CUPR_pred'] = model_cupr.predict(train)
train['PTOTAL_pred'] = model_ptotal.predict(train)

# Prepare data

X_train, X_test, y_train, y_test = train_test_split(train, y, test_size=0.25, random_state=42)

# initialize Pool

categorical_features = ['month']

train_pool = lightgbm.Dataset(X_train, 
                  y_train, categorical_feature = categorical_features)

test_pool = lightgbm.Dataset(X_test,
                 y_test) 



parameters = {
    'metric': 'rmse',
    'boosting': 'gbdt',
    'num_leaves': 31,
    'feature_fraction': 0.5,
    'bagging_fraction': 0.5,
    'bagging_freq': 20,
    'learning_rate': 0.001,
    'verbose': 0
}

model = lightgbm.train(parameters,
                       train_set = train_pool,
                       valid_sets=test_pool,
                       num_boost_round=50000,
                       early_stopping_rounds=500)




# make the prediction using the resulting model
preds = model.predict(X_test)
print(preds)
r2_score(preds, y_test)
mean_squared_error(preds, y_test)**0.5


lightgbm.plot_importance(model)

# Plot the results


plt.plot(preds, y_test, 'ro', color = 'red')
plt.show()

fig,ax = plt.subplots()
ax.scatter(y_test, preds)
ax.plot([y.min(), y.max()], [y.min(), y.max()], 'k--', lw=4)
ax.set_xlabel('Measured')
ax.set_ylabel('Predicted')
fig.show()


#Export the results

pd.DataFrame(preds).to_csv("Ligth_preds.csv")
pd.DataFrame(y_test).to_csv("y_test.csv")

preds_total = model.predict(train)

pd.DataFrame(preds_total).to_csv("preds_total.csv")

#Ggrid search

#Applying Grid Search to find the optimum set of hyperparameters

from sklearn.model_selection import GridSearchCV

model = lightgbm.LGBMRegressor(loss_function='RMSE')

parameters = {   'num_leaves': [31, 127],
                 'reg_alpha': [0.1, 0.5],
                 'min_data_in_leaf': [30, 50, 100, 300, 400],
                 'lambda_l1': [0, 1, 1.5],
                 'lambda_l2': [0, 1]
              }

grid_search = GridSearchCV(estimator=model, 
                           param_grid = parameters,
                           scoring = 'r2',
                           cv = 10)

t_start = datetime.datetime.now()
grid_search = grid_search.fit(X_train, y_train)
t_stop = datetime.datetime.now()

print(grid_search.best_params_)

#Try the optimized model


train = pd.read_csv('datos.csv')

T = pd.read_csv('T.csv')

train['T'] = T['x']

y = train['POAC']
train = train.drop(columns = ['Unnamed: 0', 'POAC', 'PTOTAL', 'PLAT', 'PLAN', 'CUPR', 'OLEA'])
train['PLAN_pred'] = model_plan.predict(train)
train['OLEA_pred'] = model_olea.predict(train)
train['CUPR_pred'] = model_cupr.predict(train)
train['PTOTAL_pred'] = model_ptotal.predict(train)

# Prepare data

X_train, X_test, y_train, y_test = train_test_split(train, y, test_size=0.25, random_state=42)

# initialize Pool

categorical_features = ['month']

train_pool = lightgbm.Dataset(X_train, 
                  y_train, categorical_feature = categorical_features)

test_pool = lightgbm.Dataset(X_test,
                 y_test) 



parameters = {
    'metric': 'rmse',
    'boosting': 'gbdt',
    'num_leaves': 31,
    'feature_fraction': 0.5,
    'bagging_fraction': 0.5,
    'bagging_freq': 20,
    'learning_rate': 0.001,
    'verbose': 0,
    'lambda_l1': 1,
    'lambda_l2': 0,
    'min_data_in_leaf': 30,
    'reg_alpha': 0.1
}

model = lightgbm.train(parameters,
                       train_set = train_pool,
                       valid_sets=test_pool,
                       num_boost_round=50000,
                       early_stopping_rounds=500)




# make the prediction using the resulting model
preds = model.predict(X_test)
print(preds)
r2_score(preds, y_test)
mean_squared_error(preds, y_test)**0.5


lightgbm.plot_importance(model)

# Plot the results


plt.plot(preds, y_test, 'ro', color = 'red')
plt.show()


fig,ax = plt.subplots()
ax.scatter(y_test, preds)
ax.plot([y.min(), y.max()], [y.min(), y.max()], 'k--', lw=4)
ax.set_xlabel('Measured')
ax.set_ylabel('Predicted')
ax.set_ylim(0,1000)
ax.set_xlim(0,1000)
fig.show()


