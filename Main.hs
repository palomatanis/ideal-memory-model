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
iterations = 10
-- iterations = 1000

memoryRange :: Int
memoryRange = 24

possible_different_addresses :: Int
possible_different_addresses = 2^(virtual_address_length - pageOffset - (virtual_address_length - memoryRange))


-- Save tests
main = do
    m <- test_complete test_reduction
    appendFile "results/sets/results_reduction.txt" ((list_to_string m) ++ "\n")
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

    
-- Creates list of n physical addresses from a virtual address, and counts eviction sets
-- The list is created by choosing n addresses with the same offset and belonging to the same range but each of a different page 
test_count_evictions :: Int -> IO(Int)
test_count_evictions number = do
  r <- list_random_sets number
  return (number_of_eviction_sets r)


-- Creates list of n sets and counts how many addresses are in eviction sets 
test_count_evictions_addresses :: Int -> IO(Int)
test_count_evictions_addresses number = do
  r <- list_random_sets number
  return (number_of_eviction_addresses r)


-- Returns 1 if there is an evicton set or 0 otherwise
test_multinomial :: Int -> IO(Int)
test_multinomial number = do
  r <- list_random_sets number
  return (bool_to_int $ exists_eviction r)


-- Creates list of sets, and a random victim set, checks if it's in an eviction set
test_binary :: Int -> IO(Int)
test_binary number = do
  r <- list_random_sets number
  v <- random_set
  return (bool_to_int $ is_address_in_eviction_set v r)


-- Creates list of sets and returns True if the reduction is successful
test_reduction :: Int -> IO (Int)
test_reduction number = do
  let free_cache = 2 ^ free_cache_bits
  r <- list_random_sets number
  v <- random_set
  return (bool_to_int $ reduction v r)

-- Creates list of sets and addreses corresponding to TLB misses and returns True if the reduction is successful
test_reduction_noisy :: Int -> IO (Int)
test_reduction_noisy number = do
  let free_cache = 2 ^ free_cache_bits
  r <- list_random_sets number
  t <- list_random_sets $ expected_tlb_misses number
  v <- random_set
  return (bool_to_int $ reduction_noisy v r t)

-- Mean of a list
mean :: [Int] -> Float
mean l = (fromIntegral $ sum l)/(fromIntegral $ length l)

bool_to_int :: Bool -> Int
bool_to_int True = 1
bool_to_int _ = 0
