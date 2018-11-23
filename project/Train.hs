module Train 
    ( predict
    , fit
    , Label
    , Input
    , Example
    , Weights
    , TrainingSet
    , TrainingConfiguration(..)
    ) where

type Label = Float
type Input = [Float]
type Example = (Input, Label)
type Weights = [Float]
type TrainingSet = [Example]

data TrainingConfiguration = TrainingConfiguration {
    initialWeights :: Weights,
    threshold :: Float,
    learningRate :: Float
} deriving Show

dot :: [Float] -> [Float] -> Float
dot a b = sum $ zipWith (*) a b

predict :: Weights -> Input -> Label
predict weights input
    | angle < 0 = -1
    | otherwise = 1
    where
        angle = dot (1 : input) weights

adjustWeights :: Example -> Float -> Weights -> Weights
adjustWeights (input, expected) learningRate weights
    | prediction == expected = weights
    | otherwise = updatedWeights
    where
        prediction = predict weights input
        mul = learningRate * (-prediction)
        scaledSample = map (mul *) (1.0 : input)
        updatedWeights = zipWith (+) weights scaledSample

epoch :: TrainingSet -> TrainingConfiguration -> (Weights, Float)
epoch trainingSet conf = (adjustedWeights, delta)
    where
        adjustedWeights = foldl (\weights example -> adjustWeights example (learningRate conf) weights) (initialWeights conf) trainingSet
        diff = zipWith (-) (initialWeights conf) adjustedWeights
        delta = sqrt $ dot diff diff

fit :: TrainingSet -> TrainingConfiguration -> Weights
fit trainingSet conf
    | converges = adjustedWeights
    | otherwise = fit trainingSet updatedConfiguration
    where
        converges = delta <= threshold conf
        (adjustedWeights, delta) = epoch trainingSet conf
        updatedConfiguration = conf { initialWeights = adjustedWeights }
