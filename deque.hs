module Deque (
  newDeque
  , isEmpty
  , addFront
  , addRear
  , removeFront
  , removeRear
  , size
) where

newtype Deque a = Deque [a] deriving (Show, Eq, Ord)

newDeque :: Deque a
newDeque = Deque []

size :: Deque a -> Int
size (Deque d) = length d

addFront :: a -> Deque a -> Deque a
addFront x (Deque d) = Deque (x:d)

addRear :: a -> Deque a -> Deque a
addRear x (Deque d) = Deque (d ++ [x])

removeFront :: Deque a -> Deque a
removeFront (Deque d) = Deque (tail d)

removeRear :: Deque a -> Deque a
removeRear (Deque d) = Deque (init d)

isEmpty :: Deque a -> Bool
isEmpty d = size d == 0
