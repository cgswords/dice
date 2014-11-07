{-# LANGUAGE NoMonomorphismRestriction #-}
import Language.Hakaru.ImportanceSampler
import Language.Hakaru.Distribution
import Language.Hakaru.Types

import qualified Data.HashMap.Strict as H
import Data.List as L
import Data.Hashable
import Control.Monad
import Data.Dynamic
import Util
import Debug.Trace

d10 = d 10

dp :: Int -> Measure Int -> Measure [Int]
dp size die = replicateM size die

explode :: Measure Int
explode = do
  d1  <- unconditioned d10 :: Measure Int
  add <- if (d1 == 10) then explode else return 0
  return $ d1 + add

main :: IO Double 
main = do l <- sample explode []
          l' <- return $ map fst l
          return $ mean $ map fromIntegral $ take 4000 l'

-- keep n k = do pool <- dp n (unconditioned d10 :: Measure Int)
keep n k = do pool <- dp n explode
              let pool' = sortBy (\ x y -> if x < y then GT else LT) pool
              let keeps = take k pool'
              return $ foldr (+) 0 keeps

main2 :: IO Double
main2 = do l <- sample (keep 5 2) []
           return $ mean $ map (fromIntegral . fst) $ take 3000 l

runKeepTest n k = do l <- sample (keep n k) []
                     return $ mean $ map (fromIntegral . fst) $ take 3000 l
                     
main3 = runKeepTest

rKT (a,b) = let (a', bp) = if a < 11 then (a,0) else (10, a-10)
                (b', bo) = if (b + bp) < 11 then (b+bp, 0) else (10, (b+bp-10)*5)
            in do x <- main3 a' b'
                  return $ (a,b,x + (fromIntegral bo))

buildRKs n = buildRKH 1 1 n
 where 
  buildRKH r k n =
    if r == n && k == n
    then [(r,k)]
    else if r == k
            then (r,k):(buildRKH (r+1) 1 n)
            else (r,k):(buildRKH r (k+1) n)
    
main4 = mapM rKT $ buildRKs 12

