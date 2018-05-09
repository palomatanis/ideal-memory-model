module Main where

import Address_translation
import Address_creation
import ReplacementPolicies

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
iterations = 50
-- iterations = 1000

memoryRange :: Int
memoryRange = 24

-- test = do
--   v <- random_set_partial
--   r <- random_cacheState v 1500
--   let i = show_cach r
--   putStrLn $ show i
  
-- Save tests
main = do
    m <- test_complete test_binary
    appendFile "results/sets/results_binary_kn_o.txt" ((list_to_string m) ++ "\n")
    --writeFile "results/sets/results_reduction_kn_5.txt" ((list_to_string m) ++ "\n")
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
  v <- random_set_partial
  r <- random_cacheState v number
  let e = evicts r lru
  return (bool_to_int e)
  

-- Creates set of addresses and returns True if the reduction is successful
test_reduction :: Int -> IO (Int)
test_reduction number = do
  v <- random_set_partial
  r <- random_cacheState v number
  red <- reduction r lru
  return (bool_to_int red)


-- Creates set of addresses and returns True if the reduction is successful
test_reductionM :: Int -> IO (Int)
test_reductionM number = do
  v <- random_set_partial
  r <- random_cacheState v number
  red <- reductionM r rr
  return (bool_to_int red)

  
-- Creates set of addresses and addreses corresponding to TLB misses and returns True if the reduction is successful
test_reduction_noisy :: Int -> IO (Int)
test_reduction_noisy number = do
  v <- random_set_partial
  r <- random_cacheState v number
  t <- new_tlb_list (expected_tlb_misses number)
  red <- reduction_noisy r t lru
  return (bool_to_int red)
  
-- Mean of a list
mean :: [Int] -> Float
mean l = (fromIntegral $ sum l)/(fromIntegral $ length l)

bool_to_int :: Bool -> Int
bool_to_int True = 1
bool_to_int _ = 0
