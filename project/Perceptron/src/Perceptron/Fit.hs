module Perceptron.Fit
    ( fit
    ) where

import Perceptron.Predict
import Perceptron.Types
import Perceptron.Util

fit :: TrainingSet -> TrainingConfiguration -> Weights
fit trainingSet conf
    | converges = adjustedWeights
    | otherwise = fit trainingSet updatedConfiguration
    where
        converges = delta <= threshold conf
        (adjustedWeights, delta) = epoch trainingSet conf
        updatedConfiguration = conf { initialWeights = adjustedWeights }

epoch :: TrainingSet -> TrainingConfiguration -> (Weights, Float)
epoch trainingSet conf = (adjustedWeights, delta)
    where
        adjustedWeights = foldl (\weights example -> adjustWeights example (learningRate conf) weights) (initialWeights conf) trainingSet
        diff = zipWith (-) (initialWeights conf) adjustedWeights
        delta = sqrt $ dot diff diff

adjustWeights :: Example -> Float -> Weights -> Weights
adjustWeights (input, expected) learningRate weights
    | prediction == expected = weights
    | otherwise = updatedWeights
    where
        prediction = predict weights input
        mul = learningRate * (-prediction)
        scaledSample = map (mul *) (1.0 : input)
        updatedWeights = zipWith (+) weights scaledSample
