import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution ((??),(?=<<) )
import Control.Monad (liftM2, replicateM)
import Data.List

type Die = Int

type Probability = Rational
type Dist = Dist.T Probability

die :: Dist Die
die = Dist.norm $ Dist.uniform [1..6]

dice :: Int -> Dist [Die]
dice n = Dist.norm $ flip replicateM die n

{- |
@hits p n@ computes the probability of getting
p hits (@>1@, @==2@, ...) when rolling n dice
-}
hits :: (Int -> Bool) -> Int -> Probability
hits p n = (p . length . filter (\ x -> x == 5 || x == 6) ) ?? dice n

main :: IO ()
main = do let res = map (\ x -> (x, mH [1..x] x)) [1..30]
          mapM (\ (x,rng) -> putStr $ (show x) ++ "," ++  
                                      (foldr (\ x ls -> show x ++ "," ++ ls) "" rng) ++ 
                                      "\n") 
               res
          return ()
  where
    mH []     n = []
    mH (r:rs) n = (hits (==r) n):(mH rs n)

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
