module Address_creation  where


import Address_translation

import System.Random
import System.Random.Shuffle

-- create list of n [0...0] addresses of length nbits (virtual)
list_of_addresses_offset_zero :: Int -> Int -> [Address]
list_of_addresses_offset_zero _ 0 = []
list_of_addresses_offset_zero nbits n = (create_address $ take nbits [0,0..0]):list_of_addresses_offset_zero nbits (n-1)

  
-- Generates list of n addesses with the same offset [0...0] in a range of 2^range addresses, each from a different page
-- Range a in bits
list_semi_random_addresses :: Int -> Int -> IO ([Address])
list_semi_random_addresses n range = do
  s <- randomIO :: IO(Int)
  let start = take (virtual_address_length - pageOffset) $ randomRs (0, 1) (mkStdGen s)
  let fixed_bits = take number_fixed_bits start
  p <- n_different_pages number_variable_bits n
  return (map create_address $ map (\x -> fixed_bits ++ x ++ offset) p)
  where
    number_fixed_bits = virtual_address_length - range
    number_variable_bits = range - pageOffset
    offset = take pageOffset [0,0..0]


n_different_pages :: Int -> Int -> IO ([[Int]])
n_different_pages n_bits len = do
  p <- shuffleM $ bins n_bits
  return (take len p)
   
-- -- Takes a number of bits, and the length  and returns a list of random, not repeated [Int]
-- n_different_pages2 :: Int -> Int -> IO ([[Int]])
-- n_different_pages2 n_bits len = n_different_pages2' n_bits len []
--   -- if ((2^n_bits) < len)
--   --   then do
--   --      p <- list_of_addresses_offset_zero n_bits len
--   --      return (map showAddress p)
--   --   else do
--   --      (return (n_different_pages' n_bits len []))
  
-- n_different_pages2' :: Int -> Int -> [[Int]] -> IO ([[Int]])
-- n_different_pages2' n_bits len original_list = do
--   let actual_list = nub original_list
--   let length_difference = len - (length actual_list)
--   if (length_difference == 0)
--     then return actual_list
--     else do
--        l <- list_of_random_addresses n_bits length_difference
--        let new_list = nub $ map showAddress l
--        r <- n_different_pages2' n_bits len (actual_list ++ new_list)
--        return r


-- create list of n random addresses of length len (virtual)
list_of_random_addresses :: Int -> Int-> IO ([Address])
list_of_random_addresses _ 0 = return []
list_of_random_addresses nbits n = do
  r <- random_address nbits
  rs <- list_of_random_addresses nbits (n-1)
  return (r:rs)
  

-- Create random address of length n
random_address :: Int -> IO (Address)
random_address nbits = do
  newRand <- randomIO
  return (createRandom_Address newRand nbits)


-- Generates a list of n seeds
generate_seeds :: Int -> IO ([Int])
generate_seeds 0 = return []
generate_seeds n = do
  r <- randomIO
  rs <- generate_seeds (n-1)
  return (r:rs)

