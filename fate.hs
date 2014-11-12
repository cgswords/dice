{-# LANGUAGE NoMonomorphismRestriction #-}
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

dF = categorical [(-1, 1),
                  (0, 1),
                  (1, 1)]

fateRoll = do d1 <- unconditioned dF
              d2 <- unconditioned dF
              d3 <- unconditioned dF
              d4 <- unconditioned dF
              return $ d1 + d2 + d3 + d4

fateRollPlus n = do f <- fateRoll
                    return $ f + n

fateReroll threshold = do f <- fateRoll 
                          if f <= threshold then fateRoll else return $ f + 2

main :: IO Double
main = do
   l <- mcmc fateRoll []
   return (mean (take 10000 l))

main2 :: IO Double
main2 = do
   l <- mcmc (fateRollPlus 2) []
   return (mean (take 10000 l))

main3 :: IO Double
main3 = do
   l <- mcmc (fateReroll (-2)) []
   return (mean (take 10000 l))  

main4 :: IO Double
main4 = do
   l <- mcmc (fateReroll (-3)) []
   return (mean (take 10000 l))  

main5 :: IO Double
main5 = do
   l <- mcmc (fateReroll (-4)) []
   return (mean (take 10000 l))  

main6 :: IO Double
main6 = do
   l <- mcmc (fateReroll (-1)) []
   return (mean (take 10000 l))  

main7 :: IO Double
main7 = do
   l <- mcmc (fateReroll 0) []
   return (mean (take 10000 l))  
