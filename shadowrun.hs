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

d6 = categorical [(1, 1),
                  (2, 1),
                  (3, 1),
                  (4, 1),
                  (5, 1),
                  (6, 1)]

prog1 = unconditioned d6

prog2  =  do
  d1 <- unconditioned d6
  d2 <- unconditioned d6
  d3 <- unconditioned d6
  return (d1+d2+d3)

prog3 = do
    d1 <- unconditioned d6 :: Measure Int
    return (if (d1 == 5 || d1 == 6) then 1 else 0)

prog4 count =
  do d1 <- unconditioned d6 :: Measure Int
     if (d1 == 5) 
     then return (count + 1)
     else if (d1 == 6)
          then (prog4 (count + 1))
          else return count

main :: IO Double
main = do
   l <- mcmc (prog4 0) []
   return (mean (take 4000 l))

dp size = do pool <- replicateM size prog3
             return $ foldr (\ x n -> x + n) 0 pool 

attackVsDodge attack dodge = 
     do attackPool <- dp attack
        dodgePool  <- dp dodge     
        let res = attack - dodge     
        return $ if (res > 0)
                 then (True, res)
                 else (False, 0)  

soakAttack attackM dmgM ap soak = do
         dmg <- dmgM
         attack <- attackM
         case attack of
            (True, res) -> do soaked <- dp (soak - ap)
                              conditioned (normal ((res + dmg) - soaked) 1)
            (False, _)  -> conditioned (normal 0 1) 
         return dmg

weaponAttack = unconditioned (uniformD 0 20)

main2 :: IO Double
main2 = do
    l <- mcmc prog3 []
    return $ mean $ take 2000 l

main4 =  do l <- mcmc (dp 10) []
            return $ mean $ take 10000 l 

main5 =  do l <- mcmc (dp 40) []
            return $ (buildPerc 10000) $ L.sort $ frequency $ take 10000 l 

main6 = do l <- mcmc (soakAttack (attackVsDodge 9 6) weaponAttack 6 24) [Just (toDyn (Lebesgue 10))]
           return $ frequency $ take 10000 l 
