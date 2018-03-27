module Address_creation  where


import Address_translation

import System.Random
import System.Random.Shuffle
import Control.Monad


list_random_sets :: Int -> IO ([Set])
list_random_sets number = replicateM number random_set

random_set :: IO (Set)
random_set = do
  r <- randomRIO(0, free_cache - 1)
  return (create_set r)

list_random_tlb :: Int -> IO ([Int])
list_random_tlb number = replicateM number $ randomRIO(0, ((2^tlb_bits) - 1))

-- create list of n [0...0] addresses of length nbits (virtual)
list_of_addresses_offset_zero :: Int -> Int -> [VAddress]
list_of_addresses_offset_zero _ 0 = []
list_of_addresses_offset_zero nbits n = (create_v_address $ take nbits [0,0..0]):list_of_addresses_offset_zero nbits (n-1)

  
-- Generates list of n addesses with the same offset [0...0] in a range of 2^range addresses, each from a different page
-- Range a in bits
list_semi_random_addresses :: Int -> Int -> IO ([VAddress])
list_semi_random_addresses n range = do
  s <- randomIO :: IO(Int)
  let start = take (virtual_address_length - pageOffset) $ randomRs (0, 1) (mkStdGen s)
  let fixed_bits = take number_fixed_bits start
  p <- n_different_pages number_variable_bits n
  return (map create_v_address $ map (\x -> fixed_bits ++ x ++ offset) p)
  where
    number_fixed_bits = virtual_address_length - range
    number_variable_bits = range - pageOffset
    offset = take pageOffset [0,0..0]


n_different_pages :: Int -> Int -> IO ([[Int]])
n_different_pages n_bits len = do
  p <- shuffleM $ bins n_bits
  return (take len p)

-- create list of n random addresses of length len (virtual)
list_of_random_addresses :: Int -> Int-> IO ([VAddress])
list_of_random_addresses _ 0 = return []
list_of_random_addresses nbits n = do
  r <- random_v_address nbits
  rs <- list_of_random_addresses nbits (n-1)
  return (r:rs)
  

-- Create random address of length n
random_v_address :: Int -> IO (VAddress)
random_v_address nbits = do
  newRand <- randomIO
  return (createRandom_VAddress newRand nbits)

  -- Create random address of length n
random_p_address :: Int -> IO (PAddress)
random_p_address nbits = do
  newRand <- randomIO
  return (createRandom_PAddress newRand nbits)


-- Generates a list of n seeds
generate_seeds :: Int -> IO ([Int])
generate_seeds 0 = return []
generate_seeds n = do
  r <- randomIO
  rs <- generate_seeds (n-1)
  return (r:rs)

