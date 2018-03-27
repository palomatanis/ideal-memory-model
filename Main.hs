module Main where

import System.Random
import System.Random.Shuffle
import Address_translation
import Control.Monad
import System.IO
import Data.List

-- media de 1000 repeticiones
-- entre 0 y 4000 direcciones. intervalo asociatividad

numberAddrToTest_From :: Int
numberAddrToTest_From = 0

numberAddrToTest_To :: Int
numberAddrToTest_To = 4000
-- numberAddrToTest_To = 4000

iterations :: Int
iterations = 100
-- iterations = 1000

memoryRange :: Int
memoryRange = 24

possible_addresses :: Int
possible_addresses = 2 ^ (virtual_address_length - pageOffset - (virtual_address_length - memoryRange))


-- printing = do
--   p <- list_semi_random_addresses 4000 memoryRange
--   putStrLn $ show $ exists_eviction p
--   --putStrLn $ to_string_addresses p

-- printing2 = do
--   p <- test_reduction_alternative 4000
--   putStrLn $ show p


-- Save tests
main = do
    m <- test_complete
    appendFile "resultsReduction_alt.txt" ((list_to_string m) ++ "\n")
    -- writeFile "resultsTest.txt" ((list_to_string m) ++ "\n")
    where list_to_string = unwords . map show

          
test_complete = do
  p <- mapM (do_test_of test_reduction_alternative) [numberAddrToTest_From, (numberAddrToTest_From + (2*associativity))..numberAddrToTest_To]
  return p

  
do_test_of :: (Int -> IO(Int)) -> Int -> IO(Float)
do_test_of f n = do
  p <- replicateM iterations $ f n
  return (mean p)


count_tlb_misses :: Int -> IO(Int)
count_tlb_misses number = do
  r <- list_semi_random_addresses number memoryRange
  return(tlb_misses r)

    
-- Creates list of n physical addresses from a virtual address, and counts eviction sets
-- The list is created by choosing n addresses with the same offset and belonging to the same range but each of a different page 
count_evictions :: Int -> IO(Int)
count_evictions number = do
  listR <- generate_seeds number
  r <- list_semi_random_addresses number memoryRange
  return (number_of_eviction_sets $ map virtual_to_physical_translation $ zip r listR)


-- Creates list of n physical addresses from a virtual address, and counts eviction sets
-- The list is created by choosing n addresses with the same offset and belonging to the same range but each of a different page 
count_addresses_evictions :: Int -> IO(Int)
count_addresses_evictions number = do
  listR <- generate_seeds number
  r <- list_semi_random_addresses number memoryRange
  return (number_of_eviction_addresses $ map virtual_to_physical_translation $ zip r listR)

multinomial_test :: Int -> IO(Int)
multinomial_test number = do
  listR <- generate_seeds number
  r <- list_semi_random_addresses number memoryRange
  return (exists_eviction $ map virtual_to_physical_translation $ zip r listR)  


-- Creates list of addresses, and a random victim address, checks if it's in an eviction set
binaryTest :: Int -> IO(Int)
binaryTest number = do
  listR <- generate_seeds number
  r <- list_semi_random_addresses number memoryRange
  let offset = take pageOffset [0,0..0]
  a <- random_address $ physical_address_length - pageOffset
  let victim = create_address $ (showAddress a) ++ offset
  return (is_address_in_eviction_set victim $ map virtual_to_physical_translation $ zip r listR)

test_reduction :: Int -> IO (Int)
test_reduction number = do
  listR <- generate_seeds number
  r <- list_semi_random_addresses number memoryRange
  let offset = take pageOffset [0,0..0]
  a <- random_address $ physical_address_length - pageOffset
  let victim = create_address $ (showAddress a) ++ offset
  return (reduction victim $ map virtual_to_physical_translation $ zip r listR)  

test_reduction_alternative :: Int -> IO (Int)
test_reduction_alternative number = do
  let free_cache_bits = cacheOffset + cacheSet - pageOffset
  let free_cache = 2 ^ free_cache_bits
  r <- replicateM number $ randomRIO(0, free_cache - 1)
  v <- randomRIO(0, free_cache - 1)
  return (reduction_alt v r)

-- create list of n [0...0] addresses of length nbits (virtual)
list_of_addresses_same_offset :: Int -> Int -> [Address]
list_of_addresses_same_offset _ 0 = []
list_of_addresses_same_offset nbits n = (create_address $ take nbits [0,0..0]):list_of_addresses_same_offset nbits (n-1)

  
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
--   --      p <- list_of_addresses_same_offset n_bits len
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


-- Mean of a list
mean :: [Int] -> Float
mean l = (fromIntegral $ sum l)/(fromIntegral $ length l)
