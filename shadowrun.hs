{-# LANGUAGE NoMonomorphismRestriction #-}

import Language.Hakaru.Metropolis
import Language.Hakaru.Distribution
import Control.Monad
import qualified Data.HashMap.Strict as H
import Data.Hashable
import Data.List as L

-- import Data.List
-- frequency :: Ord a => [a] -> [(a,Int)]
-- frequency list = map (\l@(x:xs) -> (x, length l)) (group (sort list))


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

mean l  = (sum l) / fromIntegral (length l)

d6 = categorical [(1, 1),
                  (2, 1),
                  (3, 1),
                  (4, 1),
                  (5, 1),
                  (6, 1)]

prog1 = unconditioned d6

prog2 = 
  do
    d1 <- unconditioned d6
    d2 <- unconditioned d6
    d3 <- unconditioned d6
    return (d1 + d2 + d3)

prog3 = do
  d1 <- unconditioned d6 :: Measure Int
  return (if (d1 == 5 || d1 == 6) then 1 else 0)

prog4 = \ count -> 
  do d1 <- unconditioned d6 :: Measure Int
     if (d1 == 5) 
        then return $ count + 1
        else if (d1 == 6)
             then prog4 (count + 1)
             else return $ count

dp size = do pool <- replicateM size prog3
             return $ foldr (\ x n -> x + n) 0 pool

contested :: Int -> Int -> Measure (Bool, Int)
contested attack dodge = 
 do attackPool <- dp attack
    dodgePool  <- dp dodge
    let res = attack - dodge
    return $ if (res > 0)
             then (True, res)
             else (False, 0)

soakAttack attack dmg ap soak = 
  do attack' <- attack
     case attack' of
       (True, res) -> do soaked <- dp $ soak - ap
                         return $ (res + dmg) - soaked
       (False, _)  -> return $ 0

main :: IO [(Int, Int)]
main = do
   l <- mcmc prog3 []
   return $ frequency $ take 2000 l

main2 :: IO Double
main2 = do
   l <- mcmc prog3 []
   return $ mean $ take 2000 l

main3 :: IO Double
main3 =  do l <- mcmc (prog4 0) []
            return $ mean $ take 10000 l

main4 =  do l <- mcmc (dp 40) []
            return $ mean $ take 10000 l

main5 =  do l <- mcmc (dp 40) []
            return $ (buildPerc 10000) $ L.sort $ frequency $ take 10000 l

main6 = do l <- mcmc (soakAttack (contested 9 6) 11 6 24) []            
           return $ (buildPerc 10000) $ L.sort $ frequency $ take 10000 l

doAttackVsDodge attackPool dodgePool damage ap soak = 
  do l <- mcmc (soakAttack (contested attackPool dodgePool) damage ap soak) []            
     return $ (buildPerc 10000) $ L.sort $ frequency $ take 10000 l
  

