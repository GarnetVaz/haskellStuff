import qualified Data.Map as Map
import Data.List
import qualified Data.List.Split as Split
import System.Random

simpleRolls :: (Eq a, Num a) => a -> Int -> Int -> StdGen -> ([Int], StdGen)
simpleRolls 0 l h gen = ([], gen)
simpleRolls n l h gen =
  let (value, newGen) = (randomR (l, h) gen :: (Int, StdGen))
      (value', finalGen) = simpleRolls (n-1) l h newGen
  in (value:value', finalGen)

tabNRolls :: (Num a, Num k, Ord k) => Int -> [k] -> Map.Map k a
tabNRolls n x = Map.fromListWith (+) (map (\x -> (x,1)) $ map (foldl1 (+)) $ Split.chunksOf n x)

main = do
  g <- getStdGen
  let sims = 10000
      one = 1
      six = 6
      (result, newGen) = simpleRolls sims one six g
  print $ tabNRolls 2 result
