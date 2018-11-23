module Perceptron 
    ( Perceptron
    , getWeights
    , predict
    , fit
    , Label
    , Input
    , Example
    , Weights
    , TrainingSet
    , TrainingConfiguration(..)
    ) where

import Control.Monad (guard)

import qualified Perceptron.Fit as Fit
import qualified Perceptron.Predict as Predict
import Perceptron.Types
import Perceptron.Util ((|>))

fit :: TrainingSet -> TrainingConfiguration -> Maybe Perceptron
fit trainingSet conf = do
    validateParameters trainingSet conf

    let weights = Fit.fit trainingSet conf

    return $ Perceptron weights

predict :: Perceptron -> Input -> Maybe Label
predict perceptron input
    | matchingDimensions = Just $ Predict.predict weights input
    | otherwise          = Nothing
    where
        weights = getWeights perceptron
        matchingDimensions = matchingInputAndWeightDimension input weights


validateParameters :: TrainingSet -> TrainingConfiguration -> Maybe ()
validateParameters trainingSet conf = do
    guard (learningRateIsPositive conf)
    guard (thresholdIsNonNegative conf)
    guard (hasAtLeastOneExample trainingSet)
    guard (consistentInputLength trainingSet)
    guard (appropriateBinaryLabels trainingSet)
    guard (matchingInitialWeightsLength trainingSet (initialWeights conf))

hasAtLeastOneExample :: TrainingSet -> Bool
hasAtLeastOneExample trainingSet = length trainingSet > 0

learningRateIsPositive :: TrainingConfiguration -> Bool
learningRateIsPositive conf = learningRate conf > 0

thresholdIsNonNegative :: TrainingConfiguration -> Bool
thresholdIsNonNegative conf = threshold conf >= 0

matchingInputAndWeightDimension :: Input -> Weights -> Bool
matchingInputAndWeightDimension input weights = inputAndBias == length weights
    where
        inputAndBias = 1 + length input

consistentInputLength :: TrainingSet -> Bool
consistentInputLength trainingSet =
    let
        firstInput = fst $ head trainingSet
        expectedFeatureCount = length firstInput
    in
        map fst trainingSet
        |> map length
        |> all (expectedFeatureCount ==)

appropriateBinaryLabels :: TrainingSet -> Bool
appropriateBinaryLabels trainingSet = 
    let
        isOneOrMinusOne label = abs label == 1
    in
        map snd trainingSet 
        |> all isOneOrMinusOne

matchingInitialWeightsLength :: TrainingSet -> Weights -> Bool
matchingInitialWeightsLength trainingSet initialWeights =
    let
        firstInput = fst $ head trainingSet
    in
        matchingInputAndWeightDimension firstInput initialWeights
