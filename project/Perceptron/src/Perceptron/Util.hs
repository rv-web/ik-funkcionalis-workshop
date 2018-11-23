module Perceptron.Util where

dot :: [Float] -> [Float] -> Float
dot a b = sum $ zipWith (*) a b

(|>) :: a -> (a -> b) -> b
x |> f = f x
    