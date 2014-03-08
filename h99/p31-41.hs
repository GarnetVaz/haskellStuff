-- Arithmetic problems
import Data.List (group)
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
p33 x y = if p32 x y == 1
          then True
          else False

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

p36 :: Int -> [(Int, Int)]
p36 y = map (\x -> (head x,length x)) $ group $ p35 y

----------------------
-- P37
----------------------

p37 :: Int -> Int
p37 x = sum $ map snd $ p36 x
-- Mistake in p34!

----------------------
-- P37
----------------------
