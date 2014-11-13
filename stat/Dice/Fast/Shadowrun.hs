module Dice.Fast.Shadowrun where

import Dice.Util
import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution ((??),(?=<<) )
import Control.Monad (liftM2, replicateM)
import Data.List
import qualified Data.Vector.Unboxed as V
import Control.Monad.Par

type Die = Int

type Probability = Rational
type Dist = Dist.T Probability

hitDie = Dist.relative [66.67,33.33] [0,1]
glitchDice = Dist.relative [16.67,83.33] [1,0]

dice :: Int -> Dist [Die]
dice n = replicateM n hitDie

cleanDice n = Dist.decons $ Dist.norm $ dice n

expectedVal :: Int -> Double
expectedVal n = (fromIntegral n)*0.3333333333

hitProbs :: Int -> Int -> [Double]
hitProbs big n = let allProbs = sortBy (\ (a,b) (c,d) -> if a < c then LT else GT) $
                                  map (\ (x, y) -> (sum x, fromRational y :: Double)) $ cleanDice n
                     vec      = V.accum (+) (V.replicate big (0.0 :: Double)) allProbs
                 in map roundDigits $ atLeastFreq $ V.toList vec


roundDigits n = (fromIntegral (round $ n*100000.0)) / 100000.0

hits :: IO ()
hits = do ls <- runParIO $ parMap (\ x -> (x, hitProbs 21 x)) [1..20]
          mapM (\ (x,rng) -> putStr $ (show x) ++ "," ++  
                              (foldr (\ x ls -> show x ++ "," ++ ls) "" ls) ++ "\n") 
               ls
          return ()

expectedNetHits :: Int -> [(Int, [(Int, Double)])]
expectedNetHits n = map (\ x -> (x, map (\ y -> (y, expectedVal x - expectedVal y)) [1..n])) [1..n]

nets :: IO ()
nets = do let ls = expectedNetHits 30
          mapM (\ (x,res) -> putStr $ (show x) ++ "," ++ 
                              (foldr (\ (_,x) ls -> show (roundDigits x) ++ "," ++ ls) "" res) ++ "\n")
               ls
          return ()

