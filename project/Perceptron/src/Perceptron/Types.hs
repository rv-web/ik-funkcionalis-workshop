module Perceptron.Types where

newtype Perceptron = Perceptron { getWeights :: [Float] } deriving (Show)

data TrainingConfiguration = TrainingConfiguration 
    { initialWeights :: Weights
    , threshold      :: Float
    , learningRate   :: Float
    } deriving Show

type Label = Float
type Input = [Float]
type Example = (Input, Label)
type Weights = [Float]
type TrainingSet = [Example]
    