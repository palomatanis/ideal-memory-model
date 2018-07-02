module Address_creation  where

import System.Random
import Control.Monad
import Data.Random

import Base

  
initialSet :: Set
initialSet = Set (map SetIdentifier $ take associativity $ repeat 0)


list_random_sets :: Int -> (IO (Address)) -> IO ([Address])
list_random_sets number randomizer = replicateM number randomizer


random_set_partial :: IO (Address)
random_set_partial = do
  r <- randomRIO(0, free_cache - 1)
  return (create_set r)


random_set :: IO (Address)
random_set = do
  r <- randomRIO(0, (2^cacheSet) - 1)
  return (create_set r)


list_random_tlb :: Int -> IO ([Int])
list_random_tlb number = replicateM number $ randomRIO(0, ((2^tlb_bits) - 1))


-- Creates cache state with as many congruent addresses as tlb misses
new_tlb_list :: Int -> IO(SetState)
new_tlb_list number = do
  r <- tlb_congruent number
  return (SetState (r, number))
  

tlb_congruent :: Int -> IO(Int)
tlb_congruent n = do
  r <- tlb_congruent' n 0
  return r
  where
    tlb_congruent' 0 acc = do return acc
    tlb_congruent' n acc = do
      r <- random_set
      let comp = r == (Address 0)
      rr <- tlb_congruent' (n-1) (if comp then (acc+1) else acc)
      return rr


-- Generates random cache state from victim and total number of addresses
random_cacheState :: Int -> IO(SetState)
random_cacheState number = do
  r <- random_cacheState' number 0
  return (SetState (r, number))
  where
    random_cacheState' 0 acc = do return acc
    random_cacheState' n acc = do
      r <- random_set_partial
      let comp = r == (Address 0)
      rr <- random_cacheState' (n-1) (if comp then (acc+1) else acc)
      return rr


consecutive_trace :: Int -> Trace
consecutive_trace n = Trace (map SetIdentifier [1..n])


many_consecutive_traces :: Int -> Trace -> Trace
many_consecutive_traces n (Trace t) = Trace (concat $ replicate n t)
    
-------------------------------------

-- Receives probability (out of 64) and throws a coin with that prob
chance64 :: Int -> IO(Bool)
chance64 nu = do
  let distr = (take nu $ repeat True) ++ (take (64 - nu) $ repeat False)
  r <- sample $ randomElement distr
  return r

-- Delete nth element of a list
deleteN :: Int -> [a] -> [a]
deleteN _ []  = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as

bins :: Int -> [[Int]]
bins n_bins = take (2^n_bins) $ filter((== n_bins) . length) $ concat $ iterate ((:) <$> [0, 1] <*>) [[]]

--  Like 'find', but where the test can be monadic.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p [] = return Nothing
findM p (x:xs) = ifM (p x) (return $ Just x) (findM p xs)
  where ifM b t f = do b <- b; if b then t else f
