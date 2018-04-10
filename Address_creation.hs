module Address_creation  where


import Address_translation

import System.Random
import System.Random.Shuffle
import Control.Monad


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


-- Generates random cache state from victim and total number of addresses
random_cacheState :: Address -> Int -> IO(CacheState)
random_cacheState vic number = do
  r <- random_cacheState' vic number 0
  return (CacheState (r, number))

random_cacheState' :: Address -> Int -> Int -> IO(Int)
random_cacheState' _ 0 acc = do return acc
random_cacheState' vic number acc = do
  r <- random_set_partial
  let comp = r == vic
  rr <- random_cacheState' vic (number-1) (if comp then (acc+1) else acc)
  return rr

    
