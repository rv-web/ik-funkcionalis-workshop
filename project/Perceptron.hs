type Label = Float
type Input = [Float]
type Example = (Input, Label)
type Weights = [Float]
type TrainingSet = [Example]

newtype Perceptron = Perceptron { getWeights :: [Float] } deriving (Show)

dot :: [Float] -> [Float] -> Float
dot a b = sum $ zipWith (*) a b

predict :: Perceptron -> Input -> Label
predict perceptron input
    | angle < 0 = -1
    | otherwise = 1
    where
        angle = dot (1 : input) $ getWeights perceptron

adjustWeights :: Example -> Weights -> Weights
adjustWeights (input, expected) weights
    | prediction == expected = weights
    | otherwise = updatedWeights
    where
        prediction = predict (Perceptron weights) input
        learningRate = 0.1
        mul = learningRate * (-prediction)
        scaledSample = map (mul *) (1.0 : input)
        updatedWeights = zipWith (+) weights scaledSample

epoch :: TrainingSet -> Weights -> (Weights, Float)
epoch trainingSet initialWeights = (adjustedWeights, delta)
    where
        adjustedWeights = foldl (\weights example -> adjustWeights example weights) initialWeights trainingSet
        diff = zipWith (-) initialWeights adjustedWeights
        delta = sqrt $ dot diff diff

fit :: TrainingSet -> Weights -> Float -> Perceptron
fit trainingSet initialWeights threshold
    | delta <= threshold = Perceptron adjustedWeights
    | otherwise = fit trainingSet adjustedWeights threshold
    where
        (adjustedWeights, delta) = epoch trainingSet initialWeights
