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
    , T.TrainingConfiguration(..)
    ) where

import qualified Train as T

newtype Perceptron = Perceptron { getWeights :: [Float] } deriving (Show)

matchingDimensions :: T.Input -> T.Weights -> Bool
matchingDimensions input weights = inputAndBias == length weights
    where
        inputAndBias = 1 + length input

predict :: Perceptron -> T.Input -> Maybe T.Label
predict perceptron input
    | matchingDimensions input weights = Just $ T.predict weights input
    | otherwise                        = Nothing
    where
        weights = getWeights perceptron

fit :: T.TrainingSet -> T.TrainingConfiguration -> Perceptron
fit trainingSet conf = Perceptron weights
    where
        weights = T.fit trainingSet conf
