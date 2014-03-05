module Stack (
  newStack
  , isEmpty
  , push
  , peek
  , size
  , pop
) where

newtype Stack a = Stack [a] deriving (Show, Eq, Ord)

newStack :: Stack a
newStack = Stack []

isEmpty :: Stack a -> Bool
isEmpty x = size x == 0

push :: a -> Stack a -> Stack a
push x (Stack s)= Stack (s ++ [x])

peek :: Stack a -> a
peek (Stack x)
  | length x > 0 = x !! (length x - 1)
  | otherwise = error "No elements to peek"


size :: Stack a -> Int
size (Stack x) = length x

pop :: Stack a -> (Stack a, a)
pop (Stack x) = (Stack (init x), last a)

build :: [a] -> Stack a
build [] = newStack
build x = go newStack x
  where go' s (x:xs) = go' (push x s) xs
        go' s [] = s
        go s (x:xs) = go' (push x s) xs
