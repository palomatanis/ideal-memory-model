module Main where

import Address_translation
import Address_creation

import System.Random
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
    m <- test_complete test_reduction_alternative
    appendFile "resultsReduction_alt.txt" ((list_to_string m) ++ "\n")
    -- writeFile "resultsTest.txt" ((list_to_string m) ++ "\n")
    where list_to_string = unwords . map show

          
test_complete test = do
  p <- mapM (do_test_of test) [numberAddrToTest_From, (numberAddrToTest_From + (2*associativity))..numberAddrToTest_To]
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


-- Creates list of n physical addresses from a virtual address, and counts addreses in eviction sets
-- The list is created by choosing n addresses with the same offset and belonging to the same range but each of a different page 
count_evictions_addresses :: Int -> IO(Int)
count_evictions_addresses number = do
  listR <- generate_seeds number
  r <- list_semi_random_addresses number memoryRange
  return (number_of_eviction_addresses $ map virtual_to_physical_translation $ zip r listR)

-- Returns 1 if there is an evicton set or 0 otherwise
test_multinomial :: Int -> IO(Int)
test_multinomial number = do
  listR <- generate_seeds number
  r <- list_semi_random_addresses number memoryRange
  return (exists_eviction $ map virtual_to_physical_translation $ zip r listR)  


-- Creates list of addresses, and a random victim address, checks if it's in an eviction set
test_binary :: Int -> IO(Int)
test_binary number = do
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



-- Mean of a list
mean :: [Int] -> Float
mean l = (fromIntegral $ sum l)/(fromIntegral $ length l)
