# -*- coding: utf-8 -*-
"""
Created on Thu Nov 28 11:34:40 2019

@author: Jose María
"""

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import tensorflow as tf

observations = 1000
xs = np.random.uniform(low = -10, high = 10, size = (observations,1))
zs = np.random.uniform(low = -10, high = 10, size = (observations,1))

inputs = np.column_stack((xs,zs))
inputs.shape

noise = np.random.uniform(low = -1, high = 1, size = (observations,1))

targets = 2*xs-3*zs+5+noise

init_range = 0.1

weights = np.random.uniform(low = -init_range, high = init_range, size = (2,1))
biases = np.random.uniform(low = -init_range, high = init_range, size = 1)

learning_rate = 0.02

for i in range (100):
    outputs = np.dot(inputs, weights) + biases
    deltas = outputs - targets
    
    loss = np.sum(deltas**2)/2/observations
    
    print(loss)
    
    deltas_scaled = deltas/observations
    
    weights = weights - learning_rate*np.dot(inputs.T,deltas_scaled)
    biases = biases - learning_rate*np.sum(deltas_scaled)
    