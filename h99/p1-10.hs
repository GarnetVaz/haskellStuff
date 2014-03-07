-- Problems 1 - 10 with/without using Prelude functions
import Data.List (nub, group)

--------------------------
-- P1
--------------------------

p1 :: [a] -> a
p1 [x] = x
p1 (x:xs) = p1 xs
p1 _ = error "Empty list"       -- Pattern matching in order.

p1P :: [a] -> a
p1P = last

--------------------------
-- P2
--------------------------
p2 :: [a] -> a
p2 (x:y:[]) = x
p2 (x:xs) = p2 xs
p2 _ = error "List not long enough"

p2P :: [a] -> a
p2P = head . reverse . init

--------------------------
-- P3
--------------------------

p3 :: (Num a, Ord a) => [t] -> a -> t
p3 (x:xs) n
    | n < 1 = error "Index must be greater than or equal to one"
    | n == 1 = x
    | otherwise = p3 xs $! (n-1)

p3P :: [a] -> Int -> a
p3P x n
    | n < 0 = error "Index must be greater than or equal to one"
    | n < length x = error "index too large"
    | otherwise = x !! (n-1)

--------------------------
-- P4
--------------------------

p4 :: (Integral b) => [a] -> b
p4 x = p4' x 0
    where p4' [] n = n
          p4' (x:xs) n = p4' xs $! (n+1)

p4P :: [a] -> Int
p4P = length

--------------------------
-- P5
--------------------------

p5 :: [a] -> [a]
p5 x = p5' x []
    where p5' [] y = y
          p5' (x:xs) y = p5' xs (x:y)

p5P :: [a] -> [a]
p5P = reverse

--------------------------
-- P6
--------------------------

p6 :: (Eq a) => [a] -> Bool
p6 [] = True
p6 [x] = True
p6 l@(x:xs) = (head l == last l) && p6 xs

p6P :: (Eq a) => [a] -> Bool
p6P x = x == reverse x

--------------------------
-- P7
--------------------------

data NestedList a = Elem a | List [NestedList a] deriving (Show)

-- p7 :: NestedList a -> [a]
-- p7 x = p7 x []
--     where p7 ()


-- p7P :: NestedList a -> [a]
-- p7P =

--------------------------
-- P8
--------------------------

p8 :: (Eq a) => [a] -> [a]
p8 x = p8' x []
    where p8' [x] z = reverse (x:z) -- Bad!
          p8' (x:y:ys) z = if x == y then p8' (y:ys) z else p8' (y:ys) $! x:z
          p8' _ z = z

p8P :: (Eq a) => [a] -> [a]
p8P = nub

--------------------------
-- P9
--------------------------

p9 :: (Eq a) => [a] -> [[a]]
p9 x = p9' x [] 1
    where p9' (x:y:ys) z n = if x == y
                             then p9' (y:ys) z (n+1)
                             else p9' (y:ys) (z ++ [(replicate n x)]) 1
          p9' (x:[]) z n = z ++ [(replicate n x)]

p9P :: (Eq a) => [a] -> [[a]]
p9P = group

--------------------------
-- P10
--------------------------

p10 :: (Eq a) => [a] -> [(a,Int)]
p10 x = p10 x [] 1
    where p10 (x:y:ys) z n = if x == y
                             then p10 (y:ys) z (n+1)
                             else p10 (y:ys) (z ++ [(x,n)]) 1
          p10 (x:[]) z n = z ++ [(x,n)]

p10P :: Eq a => [a] -> [(a, Int)]
p10P x = zip (fmap head g) (fmap length g)
         where g = group x
