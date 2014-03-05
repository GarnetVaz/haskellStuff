module RandomGraph (
  buildERGraph
) where

import System.Random
import Data.Graph
import qualified Data.Map.Lazy as L
import qualified Data.Map.Strict as S


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Watts-Strogatz
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

wsInit :: Int -> Int -> [(Int, Int)]
wsInit n k = let l = quot k 2
                 f = concat $ map (\x -> replicate l x) [1..n]
                 g = 2:(concat $ (map (\x -> replicate l x) [3..(n+1)])) ++ [n+2]
                 e = zip f g
                 in map (\(x,y) -> if y > n then (x,y-n) else (x,y)) e

wsShuffle :: StdGen -> Double -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
wsShuffle g p n k x = let (g', g'') = split g
                          r = take (length x) $ randoms g :: [Double]
                          f = \x -> x < p
                          l = quot k 2
                          -- pos = map (\(x,y,n) -> [x+1..y-1] ++ [y+1..n+x-1]) $ zip3 a1 a2 $ replicate n $ length x

                          -- low = [l+2..n+l+1]
                          s = map (\(r,l,u) -> floor (fromIntegral l + r* fromIntegral(u-l))) $ zip3 c a b
                      in wsFilter f r x n (quot k 2) g''



wsGenEdge :: (Int, t) -> Int -> Int -> StdGen -> ((Int, Int),StdGen)
wsGenEdge (ind, _) n l g = let (r,newG) = randomR (ind + l + 1, n + ind - 2*l + 1) g :: (Int, StdGen)
                               i = if r < n then r else (r - n)
                           in ((ind,i),g)

wsFilter  :: (t -> Bool)
     -> [t] -> [(Int, Int)] -> Int -> Int -> StdGen -> [(Int, Int)]
wsFilter _ _ [] _ _ _ = []
wsFilter _ [] _ _ _ _ = []
wsFilter p (x:xs) ((y1,y2):ys) n l g
  | p x = (y1,y2):(y2,y1) : filter' p xs ys -- Edge survived
  | otherwise = (t1,t2):(t2,t1) : filter' p xs ys             -- Replace edge
  where ((t1,t2), newG) = wsGenEdge (y1,y2) n l g
        ((t1',t2'),finalG) =


buildWSGraph g p n k = buildG bound (wsShuffle g p n k $ wsInit n k)
  where bound = (1,n)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Erdos Renyi
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
buildEREdges :: (Enum t, Num t, Ord t, RandomGen g) =>
              g -> Double -> t -> [(t, t)]

buildEREdges _ _ 0 = []
buildEREdges _ 0.0 _ = []
buildEREdges g p n = let a = [(x,y) | x <- [1..n], y <- [1..n], x < y]
                         r = take (length a) $ randoms g :: [Double]
                         f = \x -> x < p
                     in filter' f r a

filter' :: (t -> Bool) -> [t] -> [(a,a)] -> [(a,a)]
filter' _ _ [] = []
filter' _ [] _ = []
filter' p (x:xs) ((y1,y2):ys)
  | p x = (y1,y2):(y2,y1) : filter' p xs ys
  | otherwise = filter' p xs ys

buildERGraph :: RandomGen g => g -> Double -> Vertex -> Graph
buildERGraph g p n = buildG bound (buildEREdges g p n)
  where bound = (1,n)
