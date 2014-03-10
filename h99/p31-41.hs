{-# LANGUAGE BangPatterns #-}
-- Arithmetic problems

import Data.List (group, foldl')
import Control.Applicative
import Control.Arrow

----------------------
-- P31
----------------------

p31 :: Int -> Bool
p31 x = res x b 2
    where b = ceiling $ sqrt $ fromIntegral x
          res x b t
              | t == b = True
              | otherwise = ((x `mod` t) /= 0) && res x b (t+1)

----------------------
-- P32
----------------------

p32 :: Int -> Int -> Int
p32 x y = go l h
    where l = min x y
          h = max x y
          go u 0 = u
          go u v = go v (u `mod` v)

----------------------
-- P33
----------------------

p33 :: Int -> Int -> Bool
p33 x y = p32 x y == 1

----------------------
-- P34
----------------------

p34 :: Int -> Int
p34 x = length . filter (==True) $ map (p33 x) [1..x]

----------------------
-- P35
----------------------

p35 :: Int -> [Int]
p35 x = go x b 2 []
        where b = x `div` 2
              go x b t z
                 | t == (b+1) = z
                 | otherwise = if x `mod` t == 0
                                then go y b t (z ++ [t])
                                else go x b (t+1) z
                                     where y = x `div` t

----------------------
-- P36
----------------------

p36 :: Int -> [[c] -> (c, Int)]
p36 y = map (\x -> head &&& length) $ group $ p35 y

----------------------
-- P37
----------------------

-- p37 :: Int -> Int
-- p37 x = (+) 1 $ foldl' (\ acc (x,y) -> (x-1)*x^(y-1) + acc) 0 $ p36 x
-- Mistake in p34?

----------------------
-- P38
----------------------

p38 :: Integral a => a -> a -> [a]
p38 l h = go l h [2,1]
          where go !s h !z
                   | s < 2 = z
                   | s == h = z
                   | otherwise = if elem 0 $ map (rem s) [2..t]
                                 then go (s+1) h z
                                 else go (s+1) h (s:z)
                                     where t = ceiling . sqrt $ fromIntegral s

----------------------
-- P39
----------------------

p39 :: Integral t => t -> (t, t, t)
p39 x = if odd x
        then error "Number must be even"
        else let p = p38 3 x
                 r = pure (\x y -> (x+y,x,y)) <*> p <*> p
             in fst . head . filter (\((z,x,y),s) -> z == s) $ zip r (repeat x)

----------------------
-- P40
----------------------

p40 :: Integral a => a -> a -> [(a, a, a)]
p40 l h = go l h []
          where go !l h !z
                    | l > h = z
                    | otherwise = if even l
                                  then go (l+1) h (r:z)
                                  else go (l+1) h z
                                       where r = p39 l
