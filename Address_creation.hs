module Address_creation  where

import System.Random
import System.Random.Shuffle
import Control.Monad

-------------- PARAMETERS
----- Address

virtual_address_length :: Int
virtual_address_length = 48

physical_address_length :: Int
physical_address_length = 34

-- Bits of page index , between 0 and addressLength
pageOffset :: Int
pageOffset = 12

----- Cache

-- Number of offset bits (n bits for blocks of 2^n)
cacheOffset :: Int
cacheOffset = 6

-- Number of bits for set (s bits for 2^s sets)
cacheSet :: Int
cacheSet = 13

-- Actual number of cache bits that can change
free_cache_bits :: Int
free_cache_bits = cacheOffset + cacheSet - pageOffset

free_cache :: Int
free_cache = 2^free_cache_bits
  
-- -- associativity (number of blocks per cache set)
-- associativity :: Int 
-- associativity = 16


------ TLB
-- Has to be multiple of tlb associativity
tlb_size :: Int
tlb_size = 1536

-- TLB associativity (bits for tlb associativity)
tlb_bits :: Int
tlb_bits = 2


data Address = Address Int
  deriving (Read, Show, Eq)

-- The cache state is represented by the number of addresses in the victim set, and the total number of addresses
data CacheState = CacheState(Int, Int)
  deriving (Read, Show, Eq)


show_set :: Address -> Int
show_set (Address a) = a

show_cach :: CacheState -> (Int, Int)
show_cach (CacheState (a, b)) = (a, b)

create_set :: Int -> Address
create_set = Address


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
new_tlb_list :: Int -> IO(CacheState)
new_tlb_list number = do
  r <- new_tlb_list' (Address 0) number 0
  return (CacheState (r, number))

new_tlb_list' :: Address -> Int -> Int -> IO(Int)
new_tlb_list' _ 0 acc = do return acc
new_tlb_list' vic number acc = do
  r <- random_set_partial
  let comp = r == vic
  rr <- random_cacheState' vic (number-1) (if comp then (acc+1) else acc)
  return rr


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

    
