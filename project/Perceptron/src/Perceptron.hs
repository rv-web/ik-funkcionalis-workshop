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

import Control.Monad (guard)

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

fit :: T.TrainingSet -> T.TrainingConfiguration -> Maybe Perceptron
fit trainingSet conf = do
    guard (length trainingSet > 0)
    guard (T.learningRate conf > 0)
    guard (T.threshold conf >= 0)
    guard (consistentInputLength trainingSet)
    guard (appropriateBinaryLabels trainingSet)
    guard (matchingInitialWeightsLength trainingSet (T.initialWeights conf))
    let weights = T.fit trainingSet conf
    return $ Perceptron weights

(|>) :: a -> (a -> b) -> b
x |> f = f x

consistentInputLength :: T.TrainingSet -> Bool
consistentInputLength trainingSet =
    let
        firstInput = fst $ head trainingSet
        expectedFeatureCount = length firstInput
    in
        map fst trainingSet
        |> map length
        |> all (expectedFeatureCount ==)

appropriateBinaryLabels :: T.TrainingSet -> Bool
appropriateBinaryLabels trainingSet = 
    let
        isOneOrMinusOne label = abs label == 1
    in
        map snd trainingSet 
        |> all isOneOrMinusOne

matchingInitialWeightsLength :: T.TrainingSet -> T.Weights -> Bool
matchingInitialWeightsLength trainingSet initialWeights =
    let
        firstInput = fst $ head trainingSet
        expectedWeightCount = length firstInput + 1
    in
        expectedWeightCount == length initialWeights
