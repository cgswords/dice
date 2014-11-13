module Dice.Shadowrun where

import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution ((??),(?=<<) )
import Control.Monad (liftM2, replicateM)
import Data.List
import Dice.Util

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


