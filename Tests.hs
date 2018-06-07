module Main where

import Base
import Cache_model
import Address_creation
import Algorithms

import System.Random
import Control.Monad
import System.IO
import Data.List

-- media de 1000 repeticiones
-- entre 0 y 4000 direcciones. interval 2*associativity

numberAddrToTest_From :: Int
numberAddrToTest_From = 0

numberAddrToTest_To :: Int
numberAddrToTest_To = 4000

iterations :: Int
iterations = 50
-- iterations = 1000

memoryRange :: Int
memoryRange = 24
  
-- Save tests
main = do
    m <- test_complete $ test_reduction reduction lru
    writeFile "results/lru_reduction_new_noisy_changed_par_2" ((unwords $ map show m) ++ "\n")
    where
      test_complete test = do
        p <- mapM (do_test_of test) [numberAddrToTest_From, (numberAddrToTest_From + (2*associativity))..numberAddrToTest_To]
        return p
      do_test_of f n = do
        p <- replicateM iterations $ f n
        return (mean p)

-- Creates set of addresses and returns True if the reduction is successful
test_reduction :: ReductionAlgorithm -> RepPol -> Int -> IO (Int)
test_reduction reduction_alg pol number = do
  r <- random_cacheState number
  red <- reduction_alg r pol
  return (bool_to_int red)
  
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

-- test pol = do
--   l <- pol initialSet (many_consecutive_traces 5 $ consecutive_trace 20)
--   putStrLn $ show l


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
  r <- random_cacheState number
  e <- evicts r lru
  return (bool_to_int e)
  
-- Mean of a list
mean :: [Int] -> Float
mean l = (fromIntegral $ sum l)/(fromIntegral $ length l)

bool_to_int :: Bool -> Int
bool_to_int True = 1
bool_to_int _ = 0
