{-# LANGUAGE BangPatterns #-}
-- Problems 1-10
-- Brute force solutions to get accustomed to performace bottlenecks in haskell
import Data.List as L
import Data.Maybe
import qualified Data.List.Split as LS
import Control.Applicative

-- -- -- -- -- -- -- --
-- -- P1 Product of multiples of 3 or 5
-- -- -- -- -- -- -- --
p1 :: Integral a => a -> a
p1 x = product $ [y | y <- [1..x], y `mod` 3 == 0 || y `mod` 5 == 0]

-- -- -- -- -- -- -- --
-- -- P2 Sum of even fibonacci
-- -- -- -- -- -- -- --

p2 :: Integral a => a -> a
p2 x = fib' x 1 0 0
    where fib' x o1 o2 acc
              | x == 0 = 0
              | x == 1 = 0
              | o1 > x = acc
              | otherwise = fib' x (o1+o2) o1 s
              where s = if even (o1+o2)
                        then (acc + (o1+o2))
                        else acc

-- -- -- -- -- -- -- --
-- -- P3 Largest prime factor
-- -- -- -- -- -- -- --

p3 :: Int -> Int
p3 x = foldl1 max $ go x b 2 []
    where b = x `div` (ceiling $ sqrt $ fromIntegral x)
          go x b t z
              | t == (b+1) = z
              | otherwise = if x `mod` t == 0
                            then go y b t (z ++ [t])
                            else go x b (t+1) z
                                where y = x `div` t

-- -- -- -- -- -- -- --
-- -- P4 Largest palindrome of product of two 3 digit numbers
-- -- -- -- -- -- -- --

p4h :: Show a => a -> Bool
p4h x = xs == rxs
        where xs = show x
              rxs = reverse $ show x

p4 :: (Enum a, Num a, Ord a, Show a) => a -> a -> a -> a -> a
p4 xl xh yl yh = foldl1 max $ go xl xh yl yh
    where go xl xh yl yh = filter p4h $ map (\(x,y) -> x*y) [(x,y) | x <- [xl..xh], y <- [yl..yh]]

-- -- -- -- -- -- -- --
-- -- P5 Smallest multiple
-- -- -- -- -- -- -- --
p5 :: Integral a => [a] -> [a] -> Maybe Int
p5 x l = pure (+) <*> Just 1 <*> res
         where res = findIndex (==True) $ fmap (all (==0)) $ LS.chunksOf (length l) $ liftA2 rem x l

-- Alternate solution using recursion which is much faster
p5' :: Integral a => a -> [a] -> a
p5' s l = go s l
    where go s l = if all (==0) $ map (rem s) l
                   then s
                   else go (s+1) l

-- -- -- -- -- -- -- --
-- -- P6 Sum square difference
-- -- -- -- -- -- -- --

p6 :: (Enum a, Num a) => a -> a
p6 x = (-) ((^2) (foldl1 (+) [1..x])) (foldl1 (+) $ map (^2) [1..x])

-- -- -- -- -- -- -- --
-- -- P7
-- -- -- -- -- -- -- --

p7 :: (Eq a, Integral a1, Num a) => a -> a1
p7 n = go 3 [2] 1
    where go !x !z !s = if s == n
                        then head z
                        else if any (==0) $ map (rem x) [2..l]
                             then go (x+1) z s
                             else go (x+1) (x:z) (s+1)
                                 where l = ceiling $ sqrt $ fromIntegral x

-- -- -- -- -- -- -- --
-- -- P8 Largest product in series
-- -- -- -- -- -- -- --

p8 :: [Char] -> Int -> Int
p8 x n = go x n 0
    where go x n t
             | length x < n = t
             | otherwise = go (tail x) n (max t p)
             where p = product $ map (\x -> read [x] :: Int) $ take n x

-- -- -- -- -- -- -- --
-- -- P9 Special pythogorean triplet
-- -- -- -- -- -- -- --

p9 = (\(x,y,z) -> x*y*z) $ head [(a,b,c) | b <- [1..1000], a <- [1..b], let c = 1000 - a - b, a^2 + b^2 == c^2]

-- -- -- -- -- -- -- --
-- -- P10 Summation of primes
-- -- -- -- -- -- -- --

p10 :: Integral a => a -> a
p10 l = go 3 3
    where go !x !s = if x > l
                     then s
                     else if any (==0) $ map (rem x) [2..t]
                          then go (x+1) s
                          else go (x+1) (s+x)
                              where t = ceiling $ sqrt $ fromIntegral x
