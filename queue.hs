module Queue (
  newQueue
  , isEmpty
  , enqueue
  , dequeue
  , size
) where

newtype Queue a = Queue [a] deriving (Show, Eq, Ord)

newQueue :: Queue a
newQueue = Queue []

size :: Queue a -> Int
size (Queue q) = length q

isEmpty :: Queue a -> Bool
isEmpty q = size q == 0

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue q) = Queue (x:q)

dequeue :: Queue a -> (Queue a, a)
dequeue (Queue q) = (Queue(tail q), head q)
