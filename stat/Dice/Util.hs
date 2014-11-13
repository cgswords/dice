module Dice.Util where

import Data.List

atLeastFreq :: (Num a) => [a] -> [a]
atLeastFreq = foldr (\ x ls -> case ls of
                                 []     -> [x]
                                 (y:ys) -> (x+y):ls)
                         []

atMostFreq :: (Num a) => [a] -> [a]
atMostFreq = foldl' (\ ls x -> case ls of
                                []     -> [x]
                                _      -> let l = last ls
                                          in ls ++ [x+l])
                          []
