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
