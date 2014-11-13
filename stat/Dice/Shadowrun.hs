module Shadowrun where

import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution ((??),(?=<<) )
import Control.Monad (liftM2, replicateM)
import Data.List

type Die = Int

type Probability = Rational
type Dist = Dist.T Probability

hitDie = Dist.relative [66.67,33.33] [0,1]

dice :: Int -> Dist [Die]
dice n = flip replicateM hitDie n

{- |
@hits p n@ computes the probability of getting
p hits (@>1@, @==2@, ...) when rolling n dice
-}
hits :: (Int -> Bool) -> Int -> Probability
hits p n = (p . length . filter (==1)) ?? dice n

roundDigits n = (fromIntegral (round $ fromRational n*100000.0)) / 100000.0

main :: IO ()
main = do let res = map (\ x -> (x, (atLeastFreq $ map fromRational $ mH [1..x] x))) [1..30]
          mapM (\ (x,rng) -> putStr $ (show x) ++ "," ++  
                                      (foldr (\ x ls -> show (roundDigits x) ++ "," ++ ls) "" rng) ++ 
                                      "\n") 
               res
          return ()
  where
    mH []     n = []
    mH (r:rs) n = (hits (==r) n):(mH rs n)

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

-- droll :: Dist Die
-- droll =
--    liftM2 (+) (Dist.uniform [0,1]) die
-- 
-- g3 :: Probability
-- g3 = (>3) ?? die
-- 
-- addTwo :: Dist Die
-- addTwo =
--       liftM2 (+) die die
