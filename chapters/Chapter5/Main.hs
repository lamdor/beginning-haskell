module Main where

import Data.List (foldl')

main :: IO ()
main = putStrLn $ show result

-- result :: Integer
-- result = foldr (*) 1 [1 .. 100000]

result :: Integer
result = foldl' (*) 1 [1 .. 100000]
