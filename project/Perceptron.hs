dot :: [Float] -> [Float] -> Float
dot a b = sum $ zipWith (*) a b

predict :: [Float] -> [Float] -> Float
predict sample weights
    | angle < 0 = -1
    | otherwise = 1
    where
        angle = dot (1 : sample) weights

adjustWeights :: [Float] -> Float -> [Float] -> [Float]
adjustWeights sample expected weights
    | prediction == expected = weights
    | otherwise = updatedWeights
    where
        prediction = predict sample weights
        learningRate = 0.1
        mul = learningRate * (-prediction)
        scaledSample = map (mul *) (1 : sample)
        updatedWeights = zipWith (+) weights scaledSample

epoch :: [([Float], Float)] -> [Float] -> ([Float], Float)
epoch trainingSet initialWeights = (adjustedWeights, delta)
    where
        adjustedWeights = foldl (\weights (sample, expected) -> adjustWeights sample expected weights) initialWeights trainingSet
        diff = zipWith (-) initialWeights adjustedWeights
        delta = sqrt $ dot diff diff

train :: [([Float], Float)] -> [Float] -> Float -> [Float]
train trainingSet initialWeights threshold
    | delta <= threshold = adjustedWeights
    | otherwise = train trainingSet adjustedWeights threshold
    where
        (adjustedWeights, delta) = epoch trainingSet initialWeights
