module Perceptron 
    ( Perceptron
    , TrainingSet
    , TrainingConfiguration(..)
    , fit
    , getWeights
    , predict

    , Evaluate.ContingencyTable
    , Evaluate.PredictionClass(..)
    , evaluate
    , Evaluate.contingencyTable
    , Evaluate.truePositives
    , Evaluate.falsePositives
    , Evaluate.trueNegatives
    , Evaluate.falseNegatives
    , Evaluate.precision
    , Evaluate.recall
    , ValidationSet

    , Example
    , Input
    , Label
    , Weights
    ) where

import Control.Monad (guard)

import qualified Perceptron.Evaluate as Evaluate
import qualified Perceptron.Fit as Fit
import qualified Perceptron.Predict as Predict
import Perceptron.Types
import Perceptron.Util


fit :: TrainingSet -> TrainingConfiguration -> Maybe Perceptron
fit trainingSet conf = do
    validateInputSet trainingSet
    validateTrainingConfiguration trainingSet conf

    let weights = Fit.fit trainingSet conf

    return $ Perceptron weights

predict :: Perceptron -> Input -> Maybe Label
predict perceptron input
    | matchingDimensions = Just $ Predict.predict weights input
    | otherwise          = Nothing
    where
        weights = getWeights perceptron
        matchingDimensions = matchingInputAndWeightDimension input weights

evaluate :: Perceptron -> ValidationSet -> Maybe [Evaluate.PredictionClass]
evaluate perceptron validationSet = do
    validateInputSet validationSet

    let weights = getWeights perceptron
    let firstInput = fst $ head validationSet
    guard (matchingInputAndWeightDimension firstInput weights)

    return $ Evaluate.evaluate validationSet weights


validateInputSet :: [Example] -> Maybe ()
validateInputSet set = do
    guard (hasAtLeastOneExample set)
    guard (consistentInputLength set)
    guard (appropriateBinaryLabels set)

validateTrainingConfiguration :: TrainingSet -> TrainingConfiguration -> Maybe()
validateTrainingConfiguration trainingSet conf = do
    guard (learningRateIsPositive conf)
    guard (thresholdIsNonNegative conf)
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
    