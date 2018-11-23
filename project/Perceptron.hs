module Perceptron 
    ( predict
    , fit
    , Perceptron
    , getWeights
    , T.Label
    , T.Input
    , T.Example
    , T.Weights
    , T.TrainingSet
    ) where

import qualified Train as T

newtype Perceptron = Perceptron { getWeights :: [Float] } deriving (Show)

predict :: Perceptron -> T.Input -> T.Label
predict perceptron input = T.predict weights input
    where
        weights = getWeights perceptron

fit :: T.TrainingSet -> T.Weights -> Float -> Perceptron
fit trainingSet initialWeights threshold = Perceptron weights
    where
        weights = T.fit trainingSet initialWeights threshold
