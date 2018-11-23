module Perceptron.Types where

newtype Perceptron = Perceptron { getWeights :: [Float] } deriving (Show)

data TrainingConfiguration = TrainingConfiguration 
    { initialWeights :: Weights
    , threshold      :: Float
    , learningRate   :: Float
    } deriving Show

data PredictionClass = TruePositive
                     | FalsePositive
                     | TrueNegative
                     | FalseNegative
                     deriving (Eq, Show)

data ContingencyTable = ContingencyTable
    { truePositives  :: Int
    , falsePositives :: Int
    , trueNegatives  :: Int
    , falseNegatives :: Int
    } deriving (Show)

type Label = Float
type Input = [Float]
type Example = (Input, Label)
type Weights = [Float]
type TrainingSet = [Example]
type ValidationSet = [Example]
    