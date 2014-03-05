-- module Vector where

import Data.List (foldl')
import System.Random

type Scalar = Double
data Vector = Vector [Scalar] deriving (Show, Eq)

instance Num Vector where
  (+) (Vector xs) (Vector ys) = Vector (zipWith (+) xs ys)
  (-) (Vector xs) (Vector ys) = Vector (zipWith (-) xs ys)
  (*) (Vector xs) (Vector ys) = Vector (zipWith (*) xs ys)
  negate (Vector xs) = Vector (map (\x -> -1*x) xs)
  abs (Vector xs) = Vector (map (\x -> if x > 0 then x else -x) xs)
  signum = error "Not possible"
  fromInteger = error "Not possible"

instance Fractional Vector where
  (/) (Vector xs) (Vector ys) = Vector (zipWith (/) xs ys)
  recip (Vector xs) = Vector (map (\x -> 1.0/x) xs)
  fromRational = error "Not possible"

instance Floating Vector where
  exp (Vector xs)      = Vector (map (\x -> exp x) xs)
  sqrt (Vector xs)     = Vector (map (\x -> sqrt x) xs)
  log (Vector xs)      = Vector (map (\x -> log x) xs)
  sin (Vector xs)      = Vector (map (\x -> sin x) xs)
  tan (Vector xs)      = Vector (map (\x -> sin x) xs)
  cos (Vector xs)      = Vector (map (\x -> cos x) xs)
  asin (Vector xs)     = Vector (map (\x -> tan x) xs)
  atan (Vector xs)     = Vector (map (\x -> atan x) xs)
  acos (Vector xs)     = Vector (map (\x -> acos x) xs)
  sinh (Vector xs)     = Vector (map (\x -> sinh x) xs)
  tanh (Vector xs)     = Vector (map (\x -> tanh x) xs)
  cosh (Vector xs)     = Vector (map (\x -> cosh x) xs)
  asinh (Vector xs)    = Vector (map (\x -> asinh x) xs)
  atanh (Vector xs)    = Vector (map (\x -> atanh x) xs)
  acosh (Vector xs)    = Vector (map (\x -> acosh x) xs)
  -- (**) (Vector xs) pow = let f = (\p x -> x**p) pow
  --                            (Vector l) = xs
  --                        in Vector (map f l)

  pi = error "Not possible"

dot :: Vector -> Vector -> Scalar
dot v1 v2 = let (Vector l) = v1 * v2 in
   sum l

scal :: Vector -> Scalar -> Vector
scal v1 s = let f = (\x y -> x*y) s
                (Vector l) = v1
            in Vector (map f l)

axpy :: Vector -> Scalar -> Vector -> Vector
axpy v1 a v2 = (scal v1 a) + v2

norm1 :: Vector -> Scalar
norm1 v1 = let (Vector l) = v1
               in sum $ map abs l

norm2 :: Vector -> Scalar
norm2 v1 = let (Vector l) = sqrt v1
           in sum l

a = Vector (take 10 $ randoms (mkStdGen 11) :: [Double])
b = Vector (take 10 $ randoms (mkStdGen 10) :: [Double])
