{-# LANGUAGE NoMonomorphismRestriction #-}
module Shadowrun where

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

hitRoll = do
    d1 <- unconditioned d6 :: Measure Int
    return (if (d1 == 5 || d1 == 6) then 1 else 0)

edgeRoll count =
  do d1 <- unconditioned d6 :: Measure Int
     case d1 of
       5 -> return (count + 1)
       6 -> edgeRoll (count + 1)
       _ -> return count

dp size = do pool <- replicateM size hitRoll
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
                              conditioned (normal (fromIntegral ((res + dmg) - soaked)) 1)
            (False, _)  -> conditioned (normal 0 1) 
         return dmg

weaponAttack = unconditioned (uniformD 0 20)

prepRoll alch mag force = do roll <- dp $ alch + mag
                             hits <- return $ min roll force
                             resist <- dp force
                             if hits <= resist
                             then return $ 0
                             else return $ (hits - resist)

drainRoll draindp drain = do draindp <- dp draindp
                             if draindp <= drain
                             then return $ (drain - draindp)
                             else return $ 0

healRoll body will 0 = dp $ body + will
healRoll body will t = do heals <- healRoll body will (t - 1)
                          healres  <- dp $ body + will
                          return $ healres + heals

atLeastFreq :: (Num a) => [a] -> [a]
atLeastFreq = foldr (\ x ls -> case ls of
                                 []     -> [x]
                                 (y:ys) -> (x+y):ls) 
                         []

-- foldl :: (b -> a -> b) -> b -> [a] -> b

atMostFreq :: (Num a) => [a] -> [a]
atMostFreq = foldl (\ ls x -> case ls of
                               []     -> [x]
                               _      -> let l = last ls
                                         in ls ++ [x+l])
                         []


main :: IO ()
main = do res <- mapM (\ x -> do ls <- main5 x; return (x, map snd $ ls)) ([1..30] :: [Int])
          let res' = map (\ (num,ls) -> (num, atLeastFreq ls)) res
          mapM (\ y -> do putStr $ show y; putStr ['\n']) res'
          return ()

main3 n =  do l <- mcmc (dp n) []
              return $ mean $ take 100000 l 

main5 :: Int -> IO [(Int, Double)]
main5 n = do l <- mcmc (dp n) []
             return $ (buildPerc 100000) $ L.sort $ frequency $ take 100000 l 

main6 = do l <- mcmc (soakAttack (attackVsDodge 9 6) weaponAttack 6 24) [Just (toDyn (Lebesgue (10 :: Double)))]
           return $ frequency $ take 10000 l 

preps = do preps <- mcmc (prepRoll 11 6 4) [] -- Spec: Contact + Enchanting: Alchemical Focus 3
           return $ buildPerc 100000.0 $ frequency $ take 100000 preps

drain = do drains <-  mcmc (drainRoll 13 2) [] -- 8 Cha + 5 Wil
           return $ buildPerc 100000.0 $ frequency $ take 100000 drains

heal  = do healing <- mcmc (healRoll 1 5 4) []
           return $ buildPerc 100000.0 $ sortBy (\ (x,_) (y,_) -> if x < y then LT else GT) $ frequency $ take 100000 healing
