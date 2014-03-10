-- Finding all pairs distance

import Control.Applicative (liftA2)
import Data.List.Split (chunksOf)

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1,y1) (x2,y2) = sqrt . sum $ [(x1-x2)^2,(y1-y2)^2]

allPairs :: [(Double, Double)] -> [(Double, Double)] -> [Double]
allPairs = liftA2 dist

assign :: [[Double]] -> [Int]
assign x = fmap (f . flip zip [1..]) x
         where f d = snd (foldl (\(oldx,oldy) (x,y) -> if x < oldx
                                                       then (x,y)
                                                       else (oldx,oldy))
                          (100.0,-1) d)

-- main = print dists where
points = zip [1..4 :: Double] [1..4 :: Double]
centers = zip [10..12 :: Double] [10..12 :: Double]
dists = chunksOf (length centers) $ allPairs centers points
