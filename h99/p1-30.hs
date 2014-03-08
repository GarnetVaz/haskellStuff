-- Problems 1 - 10 with/without using Prelude functions
import Data.List (nub, group, sort)
import System.Random

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

--------------------------
-- P11
--------------------------

type Count = Int
data Encode a = Single a | Multiple a Count deriving (Show)

p11 :: (Eq a) => [a] -> [Encode a]
p11 x = map (\(x,y) -> if y > 1 then Multiple x y else Single x) z
        where z = p10 x

p11P :: (Eq a) => [a] -> [Encode a]
p11P x = map f $ p10P x
    where f = \(x,y) -> if y > 1 then Multiple x y else Single x

--------------------------
-- P12
--------------------------

p12 :: [Encode a] -> [a]
p12 x = p12' x []
        where
          p12' ((Multiple val count):y) z = p12' y (z ++ replicate count val)
          p12' ((Single val):y) z = p12' y (z ++ [val])
          p12' [] z = z

p12P :: [Encode a] -> [a]
p12P x = foldl1 (++) $ map f x
    where f = \t -> case t of
                      Multiple val count -> replicate count val
                      Single val -> [val]

--------------------------
-- P13 ?
--------------------------

--------------------------
-- P14
--------------------------

p14 :: [a] -> [a]
p14 x = p14' x []
    where p14' (x:y:ys) z = p14' (y:ys) (z ++ [x,x])
          p14' (x:[]) z = z ++ [x,x]
          p14' [] z = z

p14P :: [a] -> [a]
p14P x = foldl1 (++) $ map (replicate 2) x

--------------------------
-- P15
--------------------------

p15 :: [a] -> Int -> [a]
p15 x n = p15' x n []
    where p15' (x:y:ys) n z = p15' (y:ys) n (z ++ (rep n x))
          p15' (x:[]) n z = (z ++ (rep n x))
          p15' [] n z = z
          rep n x = rep' n x []
              where rep' 0 x z = z
                    rep' n x z = rep' (n-1) x (z ++ [x])

p15P :: [a] -> Int -> [a]
p15P x n = foldl1 (++) $ map (replicate n) x

--------------------------
-- P16
--------------------------

p16 :: [a] -> Int -> [a]
p16 x n = p16' x n 1 []
    where p16' (x:y) n d z = if n == d
                             then p16' y n 1 z
                             else p16' y n (d+1) (z ++ [x])
          p16' [] _ _ z = z

p16P :: [a] -> Int -> [a]
p16P x n = map fst $ filter (\(x,y) -> y `rem` n /= 0) $ zip x [1..]

--------------------------
-- P17
--------------------------

p17 :: [a] -> Int -> ([a],[a])
p17 x n
    | n < 0 = ([],x)
    | n > length x = (x,[])
    | otherwise = p17' x n 0 ([],[])
    where p17' (x:y) n c (z1,z2) = if n == c
                                   then (z1,x:y)
                                   else p17' y n (c+1) (z1 ++ [x],[])

p17P :: [a] -> Int -> ([a],[a])
p17P x n = splitAt n x

--------------------------
-- P18
--------------------------

p18 :: [a] -> Int -> Int -> [a]
p18 x l h
    | l < 0 = []
    | h > length x = x
    | otherwise = p18' x l h l []
    where p18' x l h t z = if t == h
                           then z
                           else p18' x l h (t+1) (z ++ [x !! t])

p18P :: [a] -> Int -> Int -> [a]
p18P x l h = let (_,s) = splitAt l x
                 (e,_) = splitAt (h-l) s
             in e

--------------------------
-- P19
--------------------------

p19 :: [a] -> Int -> [a]
p19 x n
    | n < 0 = p19 x (-n)        -- Check this! Rotate left?
    | n == lx = x
    | otherwise = p19' x (n `rem` lx) 0 []
    where lx = length x
          p19' l@(x:ys) n' t z = if t == n'
                                 then l ++ z
                                 else p19' ys n' (t+1) (z ++ [x])

p19P :: [a] -> Int -> [a]
p19P x n = (++) y1 y2
    where y1 = snd s
          y2 = fst s
          s = splitAt n x

--------------------------
-- P20
--------------------------

p20 :: [a] -> Int -> [a]
p20 x n
    | n < 0 = x
    | n > lx = x
    | otherwise = p20' x n 1 []
    where lx = length x
          p20' (y:ys) n t z = if t == n
                              then z ++ ys
                              else p20' ys n (t+1) (z ++ [y])

p20P :: [a] -> Int -> [a]
p20P x n
    | n < 0 = x
    | n > length x = x
    | otherwise = (++) l h
    where (s,h) = splitAt n x
          l = init s

--------------------------
-- P21
--------------------------

p21 :: [a] -> Int -> a -> [a]
p21 x n v
    | n < 0 = v:x
    | n > length x = x ++ [v]
    | otherwise = (++) l (v:h)
    where (l,h) = p21' x n 1 []
          p21' r@(x:y) n t z = if n == t
                               then (z,r)
                               else p21' y n (t+1) (z ++ [x])

p21P :: [a] -> Int -> a -> [a]
p21P x n v
     | n < 0 = v:x
     | n - 1 > length x = x ++ [v]
     | otherwise = (++) l (v:h)
     where (l,h) = splitAt (n-1) x

--------------------------
-- P22
--------------------------

p22 :: Int -> Int -> [Int]
p22 l h
    | h < l = error "Max must be more than min"
    | otherwise = [l..h]

--------------------------
-- P23
--------------------------

p23 :: (Ord a, RandomGen g) => g -> [a] -> Int -> [a]
p23 s x n = let
    lx = length x
    z = sort $ flip zip x (take lx $ randoms s :: [Double])
    in take n $ map snd z
            -- Need to change this to with replacement
--------------------------
-- P24
--------------------------

p24 :: (Ord a, RandomGen g) => g -> [a] -> Int -> [a]
p24 s x n
    | n >= length x = x
    | otherwise = let
                  lx = length x
                  z = sort $ flip zip x (take lx $ randoms s :: [Double])
                  in take n $ map snd z
                     -- Without replacement
--------------------------
-- P25
--------------------------

p25 :: (Ord a, RandomGen g) => g -> [a] -> [a]
p25 s x = p23 s x (length x)

--------------------------
-- P26
--------------------------

--------------------------
-- P27
--------------------------

--------------------------
-- P28
--------------------------

--------------------------
-- P29
--------------------------

--------------------------
-- P30
--------------------------
