module Main where

import Control.Monad (guard)
import System.Environment (getArgs)

import DataSet (readDataSet)

import Perceptron
    ( TrainingSet
    , TrainingConfiguration(..)
    , fit

    , ValidationSet
    , ContingencyTable
    , contingencyTable
    , evaluate
    )


main :: IO ()
main = do
    [trainingPath, validationPath] <- getArgs

    trainingSet <- readDataSet trainingPath
    validationSet <- readDataSet validationPath

    case fitAndValidate trainingSet validationSet of
        Nothing -> putStrLn "Could not fit and validate"
        Just table -> putStrLn $ show table


makeTrainingConfiguration :: TrainingSet -> Maybe TrainingConfiguration
makeTrainingConfiguration trainingSet = do
    guard (length trainingSet > 0)

    let featureCount = length $ fst $ head trainingSet

    return TrainingConfiguration
        { initialWeights = replicate (featureCount + 1) 0
        , learningRate   = 0.1
        , threshold      = 0.1
        }

fitAndValidate :: TrainingSet -> ValidationSet -> Maybe ContingencyTable
fitAndValidate trainingSet validationSet = do
    conf <- makeTrainingConfiguration trainingSet
    model <- fit trainingSet conf
    classes <- evaluate model validationSet
    return $ contingencyTable classes
    