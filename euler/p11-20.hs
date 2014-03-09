{-# LANGUAGE BangPatterns #-}

-- -- -- -- -- -- -- --
-- -- P11
-- -- -- -- -- -- -- --

-- -- -- -- -- -- -- --
-- -- P12 Highly divisible triangular numbers
-- -- -- -- -- -- -- --

p12 :: Integral a => Int -> a
p12 n = go 2 n
    where go !t n
              | n < s = t
              | otherwise = go (t+1) n
              where s = length $ filter (==0) $ z
                    z = map (rem t) [1..l]
                    l = div t 2

-- Need to use memoization. Too slow

-- -- -- -- -- -- -- --
-- -- P13 Large sum
-- -- -- -- -- -- -- --

p13 :: Num a => [a] -> a
p13 x = foldl1 (+) x

-- -- -- -- -- -- -- --
-- -- P14 Longest colatz sequence
-- -- -- -- -- -- -- --

-- Use memoization again
-- Run only compiled version with -O2
-- How granular should the memoization be?
p17 n = go 1 n 0 0
    where go !x n !m !v
              | x > n = (v,m)
              | otherwise = let m1 = p17' x
                            in if m1 > m
                               then go (x+1) n m1 x
                               else go (x+1) n m v

p17' x = go x 0
        where go !x !l
                 | x == 1 = l
                 | odd x = go (3*x + 1) (l+1)
                 | otherwise = go (quot x 2) (l+1)

-- -- -- -- -- -- -- --
-- -- P15 Lattice paths
-- -- -- -- -- -- -- --

-- Can be computed by hand

-- -- -- -- -- -- -- --
-- -- P16 Power digit sum
-- -- -- -- -- -- -- --

p16 :: Int -> Int
p16 n = foldl1 (+) $ map (\x -> read [x] :: Int) $ show $ 2^n

-- -- -- -- -- -- -- --
-- -- P17 Number letter counts
-- -- -- -- -- -- -- --

-- No

-- -- -- -- -- -- -- --
-- -- P18 Maximum path sum
-- -- -- -- -- -- -- --

-- implement data type for dynamic programming

-- -- -- -- -- -- -- --
-- -- P19
-- -- -- -- -- -- -- --

-- No

-- -- -- -- -- -- -- --
-- -- P20
-- -- -- -- -- -- -- --

p20 n = foldl1 (+) $ map (\x -> read [x] :: Int) $ show $ product [1..100]
