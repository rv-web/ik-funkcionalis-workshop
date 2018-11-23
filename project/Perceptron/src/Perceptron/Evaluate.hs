module Perceptron.Evaluate
    ( PredictionClass(..)
    , evaluate
    , precision
    , recall
    , contingencyTable
    , ContingencyTable
    , truePositives
    , falsePositives
    , trueNegatives
    , falseNegatives
    ) where

import Perceptron.Predict
import Perceptron.Types
import Perceptron.Util

emptyContingencyTable :: ContingencyTable
emptyContingencyTable = ContingencyTable 
    { truePositives  = 0
    , falsePositives = 0
    , trueNegatives  = 0
    , falseNegatives = 0
    }

evaluate :: ValidationSet -> Weights -> [PredictionClass]
evaluate validationSet weights =
    predictAll validationSet weights
    |> map evaluatePrediction

contingencyTable :: [PredictionClass] -> ContingencyTable
contingencyTable predictions = foldl inc emptyContingencyTable predictions
    where
        inc table TruePositive  = table { truePositives  = truePositives  table + 1 }
        inc table FalsePositive = table { falsePositives = falsePositives table + 1 }
        inc table TrueNegative  = table { trueNegatives  = trueNegatives  table + 1 }
        inc table FalseNegative = table { falseNegatives = falseNegatives table + 1 }

precision :: ContingencyTable -> Float
precision table = tp / (tp + fp)
    where
        tp = fromIntegral $ truePositives table
        fp = fromIntegral $ falsePositives table

recall :: ContingencyTable -> Float
recall table = tp / (tp + fn)
    where
        tp = fromIntegral $ truePositives table
        fn = fromIntegral $ falseNegatives table


predictAll :: ValidationSet -> Weights -> [(Label, Label)]
predictAll validationSet weights =
    map (\(input, expected) -> (expected, predict weights input)) validationSet

evaluatePrediction :: (Label, Label) -> PredictionClass
evaluatePrediction (expected, actual) = 
    case expected of
        1  -> if actual == 1 then TruePositive else FalseNegative
        -1 -> if actual == -1 then TrueNegative else FalsePositive
