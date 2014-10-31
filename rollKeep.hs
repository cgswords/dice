{-# LANGUAGE NoMonomorphismRestriction #-}
import Language.Hakaru.Metropolis
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

explode :: Measure Int
explode = do
  d1  <- unconditioned d10 :: Measure Int
  add <- if (d1 == 10) then explode else return 0
  return $ d1 + add

main :: IO Double 
main = do l <- mcmc explode []
          return $ mean $ map fromIntegral $ take 4000 l

dp :: Int -> Measure [Int]
dp size = replicateM size explode

keep n k = do pool <- dp n
              let pool' = sortBy (\ x y -> if x < y then GT else LT) pool
              let keeps = take k pool
              return $ foldr (+) 0 $ trace (show keeps) $ keeps

main2 :: IO Double
main2 = do l <- mcmc (keep 5 2) []
           return $ mean $ map fromIntegral $ take 30000 l

-- prog4 count =
--   do d1 <- unconditioned d6 :: Measure Int
--      if (d1 == 5) 
--      then return (count + 1)
--      else if (d1 == 6)
--           then (prog4 (count + 1))
--           else return count
-- 
-- main :: IO Double
-- main = do
--    l <- mcmc (prog4 0) []
--    return (mean (take 4000 l))
-- 
-- dp size = do pool <- replicateM size prog3
--              return $ foldr (\ x n -> x + n) 0 pool 
-- 
-- attackVsDodge attack dodge = 
--      do attackPool <- dp attack
--         dodgePool  <- dp dodge     
--         let res = attack - dodge     
--         return $ if (res > 0)
--                  then (True, res)
--                  else (False, 0)  
-- 
-- soakAttack attackM dmgM ap soak = do
--          dmg <- dmgM
--          attack <- attackM
--          case attack of
--             (True, res) -> do soaked <- dp (soak - ap)
--                               conditioned (normal (fromIntegral ((res + dmg) - soaked)) 1)
--             (False, _)  -> conditioned (normal 0 1) 
--          return dmg
-- 
-- weaponAttack = unconditioned (uniformD 0 20)
-- 
-- main2 :: IO Double
-- main2 = do
--     l <- mcmc prog3 []
--     return $ mean $ take 2000 l
-- 
-- main4 =  do l <- mcmc (dp 10) []
--             return $ mean $ take 10000 l 
-- 
-- main5 =  do l <- mcmc (dp 40) []
--             return $ (buildPerc 10000) $ L.sort $ frequency $ take 10000 l 
-- 
-- main6 = do l <- mcmc (soakAttack (attackVsDodge 9 6) weaponAttack 6 24) [Just (toDyn (Lebesgue (10 :: Double)))]
--            return $ frequency $ take 10000 l 
