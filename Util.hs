{-# LANGUAGE NoMonomorphismRestriction #-}

module Util where

import Language.Hakaru.Metropolis
import Language.Hakaru.Distribution
import Language.Hakaru.Types

import qualified Data.HashMap.Strict as H
import Data.List as L
import Data.Hashable
import Control.Monad
import Data.Dynamic

frequency :: (Eq a, Hashable a) => [a] -> [(a, Int)]
frequency [] = [] 
frequency ls = freqs ls H.empty   
  where     
   freqs [] ans = H.toList $ ans     
   freqs (x:ls) ans = freqs ls $ put x ans     
   put x map = case (H.lookup x map) of                    
      (Just i)  -> H.insert x (i + 1) map
      (Nothing) -> H.insert x 1 map

buildPerc :: Double -> [(a, Int)] -> [(a, Double)] 
buildPerc n []           = []
buildPerc n ((x,rhs):xs) = ((x,(fromIntegral rhs)/n):buildPerc n xs) 

mean l = (sum l) / fromIntegral (length l)

d n = categorical $ zip [1..n] $ repeat 1

