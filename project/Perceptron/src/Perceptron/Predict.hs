module Perceptron.Predict 
    ( predict
    ) where

import Perceptron.Types
import Perceptron.Util

predict :: Weights -> Input -> Label
predict weights input
    | angle < 0 = -1
    | otherwise = 1
    where
        angle = dot (1 : input) weights
