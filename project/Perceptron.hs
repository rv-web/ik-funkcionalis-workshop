type Label = Float
type Input = [Float]
type Example = (Input, Label)
type Weights = [Float]
type TrainingSet = [Example]

dot :: [Float] -> [Float] -> Float
dot a b = sum $ zipWith (*) a b

predict :: Input -> Weights -> Label
predict input weights
    | angle < 0 = -1
    | otherwise = 1
    where
        angle = dot (1 : input) weights

adjustWeights :: Example -> Weights -> Weights
adjustWeights (input, expected) weights
    | prediction == expected = weights
    | otherwise = updatedWeights
    where
        prediction = predict input weights
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

train :: TrainingSet -> Weights -> Float -> Weights
train trainingSet initialWeights threshold
    | delta <= threshold = adjustedWeights
    | otherwise = train trainingSet adjustedWeights threshold
    where
        (adjustedWeights, delta) = epoch trainingSet initialWeights
