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
numberAddrToTest_To = 6000

iterations :: Int
iterations = 10
-- iterations = 1000

memoryRange :: Int
memoryRange = 24

test = do
    r <- list_random_sets 4000 random_set_partial
    v <- random_set_partial
    res <- reduction v r
    putStrLn $ show res

test2 = do
    r <- list_random_sets 4000 random_set_partial
    v <- random_set_partial
    let number_addresses = length r
    e <- findM (\s -> evicts2 s v) $ reduction_combinations r
    putStrLn $ show e
        

-- test3 :: Int -> IO(Bool)
-- test3 pr = do
--     e <- chance pr
--     return e
    
-- it iterations pr = do
--   p <- replicateM iterations $ test3 pr
--   let r = length $ filter (== True) p
--   putStrLn $ show r
  
-- Save tests
main = do
    m <- test_complete test_reduction
    appendFile "results/sets/results_reduction_4.txt" ((list_to_string m) ++ "\n")
    -- writeFile "resultsTest.txt" ((list_to_string m) ++ "\n")
    where list_to_string = unwords . map show
          
test_complete test = do
  p <- mapM (do_test_of test) [numberAddrToTest_From, (numberAddrToTest_From + (2*associativity))..numberAddrToTest_To]
  return p
  
do_test_of :: (Int -> IO(Int)) -> Int -> IO(Float)
do_test_of f n = do
  p <- replicateM iterations $ f n
  return (mean p)


-- Counts TLB misses for a number of addresses
count_tlb_misses :: Int -> IO(Int)
count_tlb_misses number = do
  r <- list_random_tlb number
  return (tlb_misses r)

    
-- Creates set of number addresses and counts eviction sets
test_count_evictions :: Int -> IO(Int)
test_count_evictions number = do
  r <- list_random_sets number random_set_partial
  return (number_of_eviction_sets r)


-- Creates set of n addresses and counts how many addresses are in eviction sets 
test_count_evictions_addresses :: Int -> IO(Int)
test_count_evictions_addresses number = do
  r <- list_random_sets number random_set_partial
  return (number_of_eviction_addresses r)


-- Creates set of addresses and returns True if there's an eviction set
test_multinomial :: Int -> IO(Int)
test_multinomial number = do
  r <- list_random_sets number random_set_partial
  return (bool_to_int $ exists_eviction r)


-- Creates set of addresses, and a random victim, checks if the set is an eviction set for the victim
test_binary :: Int -> IO(Int)
test_binary number = do
  r <- list_random_sets number random_set_partial
  v <- random_set_partial
  return (bool_to_int $ evicts r v)

-- Creates set of addresses, and a random victim, checks if the set is an eviction set for the victim
test_binary2 :: Int -> IO(Int)
test_binary2 number = do
  r <- list_random_sets number random_set_partial
  v <- random_set_partial
  e <- evicts2 r v
  return (bool_to_int e)
  

-- Creates set of addresses and returns True if the reduction is successful
test_reduction :: Int -> IO (Int)
test_reduction number = do
  let free_cache = 2 ^ free_cache_bits
  r <- list_random_sets number random_set_partial
  v <- random_set_partial
  red <- reduction v r
  return (bool_to_int red)

-- Creates set of addresses and returns True if the reduction is successful
test_reduction_original :: Int -> IO (Int)
test_reduction_original number = do
  let free_cache = 2 ^ free_cache_bits
  r <- list_random_sets number random_set_partial
  v <- random_set_partial
  return (bool_to_int $ reduction_original v r)
  
-- -- Creates set of addresses and addreses corresponding to TLB misses and returns True if the reduction is successful
-- test_reduction_noisy :: Int -> IO (Int)
-- test_reduction_noisy number = do
--   let free_cache = 2 ^ free_cache_bits
--   r <- list_random_sets number random_set_partial
--   t <- list_random_sets (expected_tlb_misses number) random_set
--   v <- random_set_partial
--   return (bool_to_int $ reduction_noisy v r t)

-- Mean of a list
mean :: [Int] -> Float
mean l = (fromIntegral $ sum l)/(fromIntegral $ length l)

bool_to_int :: Bool -> Int
bool_to_int True = 1
bool_to_int _ = 0
