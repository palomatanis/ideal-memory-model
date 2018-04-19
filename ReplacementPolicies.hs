module ReplacementPolicies where

import Data.List

associativity :: Int
associativity = 16

data SetAddress = SetAddress Int
  deriving (Read, Show, Eq)
  
data Trace = Trace [SetAddress]
  deriving (Read, Show, Eq)

data Set = Set [SetAddress]
  deriving (Read, Show, Eq)

data HitNumber = Hit Int
  deriving (Read, Show, Eq)

data BTree = Tree (Node Bin, Tree BTree, Tree BTree) | Tree (Node Bin, Leaf SetAddress, Leaf SetAddress)
  deriving (Read, Show, Eq)

data Bin = Zero | One

lru :: Trace -> (Set, HitNumber)
lru trace = (s, h)
  where (t, s, h) = lru'(trace, initialSet, Hit 0)

lru' :: (Trace, Set, HitNumber) -> (Trace, Set, HitNumber)
lru' i@(Trace [], _, _) = i
lru' (Trace trace, Set set, Hit hit) =
  case (elemIndex h set) of
    Just elem -> lru'(Trace (tail trace), Set(h : (deleteN elem set)), Hit (hit + 1))
    Nothing -> lru'(Trace (tail trace), Set(h : init set), Hit hit)
  where h = head trace


fifo :: Trace -> (Set, HitNumber)
fifo trace = (s, h)
  where (t, s, h) = fifo'(trace, initialSet, Hit 0)

fifo' :: (Trace, Set, HitNumber) -> (Trace, Set, HitNumber)
fifo' i@(Trace [], _, _) = i
fifo' (Trace trace, Set set, Hit hit) =
  case (elemIndex h set) of
    Just _ -> fifo'(Trace (tail trace), Set set, Hit (hit + 1))
    Nothing -> fifo'(Trace (tail trace), Set(h : init set), Hit hit)
  where h = head trace


lip :: Trace -> (Set, HitNumber)
lip trace = (s, h)
  where (t, s, h) = lip'(trace, initialSet, Hit 0)

lip' :: (Trace, Set, HitNumber) -> (Trace, Set, HitNumber)
lip' i@(Trace [], _, _) = i
lip' (Trace trace, Set set, Hit hit) =
  case (elemIndex h set) of
    Just elem -> lip'(Trace (tail trace), Set (if (elem == ((length set) - 1)) then (h : (init set)) else set), Hit (hit + 1))
    Nothing -> lip'(Trace (tail trace), Set (init set ++ [h]), Hit hit)
  where h = head trace

plru :: Trace -> (Set, HitNumber)
plru trace = (s, h)
  where (t, s, h) = plru'(trace, initialSet, Hit 0)

plru' :: (Trace, Set, HitNumber) -> (Trace, Set, HitNumber)
plru' i@(Trace [], _, _) = i
plru' (Trace trace, Set set, Hit hit) =
  case (elemIndex h set) of
    Just elem -> plru'(Trace (tail trace), Set(h : (deleteN elem set)), Hit (hit + 1))
    Nothing -> plru'(Trace (tail trace), Set(h : init set), Hit hit)
  where h = head trace

  
-------------------------------------

initialSet :: Set
initialSet = Set (map SetAddress $ take associativity $ repeat 0)


-- Delete nth element of a list
deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as
