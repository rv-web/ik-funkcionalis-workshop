# -*- coding: utf-8 -*-
from random import shuffle

import numpy as np
import matplotlib.pyplot as plt

def generateSamples(mean, cov, size, label):
    return np.random.multivariate_normal(mean, cov, size)

def addLabel(samples, label):
    elements = []
    
    for sample in samples:
        elements.append(np.asarray(np.append(sample, label)))
        
    return elements

def trainValidateSplit(elements, trainRatio):
    index = int(len(elements) * trainRatio)
    
    train = elements[:index]
    validate = elements[index:]
    
    return (train, validate)

def saveAsCsv(path, elements):
    np.savetxt(path, elements, delimiter=",", fmt="%f")


classOne = generateSamples([10, 10], [[1, 0],[0, 1]], 1000, 1)
plt.plot(classOne[:, 0], classOne[:, 1], '.')
(trainOne, validateOne) = trainValidateSplit(addLabel(classOne, 1), 0.6)

classMinusOne = generateSamples([-10, -10], [[1, 0],[0, 1]], 1000, -1)
(trainMinusOne, validateMinusOne) = trainValidateSplit(addLabel(classMinusOne, -1), 0.6)
plt.plot(classMinusOne[:, 0], classMinusOne[:, 1], '.')

train = trainOne + trainMinusOne
validate = validateOne + validateMinusOne

shuffle(train)
shuffle(validate)

saveAsCsv("separable-train.csv", train)
saveAsCsv("separable-validate.csv", validate)