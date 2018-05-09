module ReplacementPolicies where

import Data.List
import System.Random
import Data.Random


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

-- data BTree = Tree (Node Bin, Tree BTree, Tree BTree) | Tree (Node Bin, Leaf SetAddress, Leaf SetAddress)
--   deriving (Read, Show, Eq)

data Bin = Zero | One

type RepPol = Set -> Trace -> (Set, HitNumber)
type RepPolM = Set -> Trace -> IO(Set, HitNumber)

 -- Least recently used
lru :: RepPol
lru set trace = (s, h)
  where (t, s, h) = lru'(trace, set, Hit 0)

lru' :: (Trace, Set, HitNumber) -> (Trace, Set, HitNumber)
lru' i@(Trace [], _, _) = i
lru' (Trace trace, Set set, Hit hit) =
  case (elemIndex h set) of
    Just elem -> lru'(Trace (tail trace), Set(h : (deleteN elem set)), Hit (hit + 1))
    Nothing -> lru'(Trace (tail trace), Set(h : init set), Hit hit)
  where h = head trace


-- Most recently used
mru :: RepPol
mru set trace = (s, h)
  where (t, s, h) = mru'(trace, set, Hit 0)

mru' :: (Trace, Set, HitNumber) -> (Trace, Set, HitNumber)
mru' i@(Trace [], _, _) = i
mru' (Trace trace, Set set, Hit hit) =
  case (elemIndex h set) of
    Just elem -> mru'(Trace (tail trace), Set((deleteN elem set) ++ [h]), Hit (hit + 1))
    Nothing ->
      case (elemIndex (SetAddress 0) set) of
         Just elem -> mru'(Trace (tail trace), Set((deleteN elem set) ++ [h]), Hit hit)
         Nothing ->  mru'(Trace (tail trace), Set((init set) ++ [h]), Hit hit)
  where h = head trace


-- Random replacement
rr :: RepPolM
rr set trace = do
  (t, s, h) <- rr'(trace, set, Hit 0)
  return (s, h)

rr' :: (Trace, Set, HitNumber) -> IO(Trace, Set, HitNumber)
rr' i@(Trace [], _, _) = do return i
rr' (Trace trace, Set set, Hit hit) = 
  case (elemIndex h set) of
    Just elem -> do
      r <- rr'(Trace (tail trace), Set set, Hit (hit + 1))
      return r
    Nothing ->
      case (elemIndex (SetAddress 0) set) of
        Just elem -> do
          r <- rr'(Trace (tail trace), Set(h : (deleteN elem set)), Hit hit)
          return r
        Nothing -> do
          b <- randomRIO(0, (associativity - 1))
          r <- rr' (Trace (tail trace), Set (h : (deleteN b set)), Hit hit)
          return r
  where h = head trace


-- First in first out
fifo :: RepPol
fifo set trace = (s, h)
  where (t, s, h) = fifo'(trace, set, Hit 0)

fifo' :: (Trace, Set, HitNumber) -> (Trace, Set, HitNumber)
fifo' i@(Trace [], _, _) = i
fifo' (Trace trace, Set set, Hit hit) =
  case (elemIndex h set) of
    Just _ -> fifo'(Trace (tail trace), Set set, Hit (hit + 1))
    Nothing -> fifo'(Trace (tail trace), Set(h : init set), Hit hit)
  where h = head trace


-- LRU insertion policy
lip :: RepPol
lip set trace = (s, h)
  where (t, s, h) = lip'(trace, set, Hit 0)

lip' :: (Trace, Set, HitNumber) -> (Trace, Set, HitNumber)
lip' i@(Trace [], _, _) = i
lip' (Trace trace, Set set, Hit hit) =
  case (elemIndex h set) of
    Just elem -> lip'(Trace (tail trace), Set (if (elem == ((length set) - 1)) then (h : (init set)) else set), Hit (hit + 1))
    Nothing -> lip'(Trace (tail trace), Set (init set ++ [h]), Hit hit)
  where h = head trace


-- Bimodal insertion policy
bip :: RepPolM
bip set trace = do
  (t, s, h) <- bip'(trace, set, Hit 0)
  return (s, h)

bip' :: (Trace, Set, HitNumber) -> IO(Trace, Set, HitNumber)
bip' i@(Trace [], _, _) = do return i
bip' (Trace trace, Set set, Hit hit) = 
  case (elemIndex h set) of
    Just elem -> do
      r <- bip'(Trace (tail trace), Set (if (elem == ((length set) - 1)) then (h : (init set)) else set), Hit (hit + 1))
      return r
    Nothing ->
      case (elemIndex (SetAddress 0) set) of
        Just elem -> do
          r <- bip'(Trace (tail trace), Set(h : (deleteN elem set)), Hit hit)
          return r
        Nothing -> do
          b <- chance64 2
          if b
            then bip'(Trace (tail trace), Set (h : init set), Hit hit)
            else bip'(Trace (tail trace), Set (init set ++ [h]), Hit hit)
  where h = head trace



-- plru :: Set -> Trace -> (Set, HitNumber)
-- plru trace = (s, h)
--   where (t, s, h) = plru'(trace, initialSet, Hit 0)

-- plru' :: (Trace, Set, HitNumber) -> (Trace, Set, HitNumber)
-- plru' i@(Trace [], _, _) = i
-- plru' (Trace trace, Set set, Hit hit) =
--   case (elemIndex h set) of
--     Just elem -> plru'(Trace (tail trace), Set(h : (deleteN elem set)), Hit (hit + 1))
--     Nothing -> plru'(Trace (tail trace), Set(h : init set), Hit hit)
--   where h = head trace

  
-------------------------------------
-- Receives probability and throws a coin with that prob
chance64 :: Int -> IO(Bool)
chance64 nu = do
  let distr = (take nu $ repeat True) ++ (take (64 - nu) $ repeat False)
  r <- sample $ randomElement distr
  return r
  
initialSet :: Set
initialSet = Set (map SetAddress $ take associativity $ repeat 0)


-- Delete nth element of a list
deleteN :: Int -> [a] -> [a]
deleteN _ []  = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as
