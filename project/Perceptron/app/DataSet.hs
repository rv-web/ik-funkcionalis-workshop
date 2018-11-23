module DataSet
    ( readDataSet
    ) where

import Data.List.Split (splitOn)

import Perceptron (Example)

readDataSet :: FilePath -> IO [Example]
readDataSet path = parseDataSet <$> readFile path 

parseDataSet :: String -> [Example]
parseDataSet = map parseExample . lines

parseExample :: String -> Example
parseExample line = (input, label)
    where
        entries = reverse $ splitOn "," line
        label = read $ head entries
        input = map read $ tail entries
