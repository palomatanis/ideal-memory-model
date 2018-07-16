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

rangeTests :: Int
rangeTests = 2 * associativity

iterations :: Int
iterations = 1000

path :: String
path = "testpath"

-- Save tests
main = do
  m <- test_complete $ test_binary lru
  writeFile path ((unwords $ map show m) ++ "\n")
    where
      test_complete test = do
        p <- mapM (do_test_of test) [numberAddrToTest_From, (numberAddrToTest_From + rangeTests)..numberAddrToTest_To]
        return p
      do_test_of f n = do
        p <- replicateM iterations $ f n
        return (mean p)

-- Creates set of addresses and returns True if the reduction is successful
test_reduction :: ReductionAlgorithm -> RepPol -> Int -> IO (Int)
test_reduction reduction_alg pol number = do
  r <- random_SetOfAddresses number
  red <- reduction_alg r pol
  return (maybe_to_int red)
  
-- Counts TLB misses for a number of addresses
count_tlb_misses :: Int -> IO(Int)
count_tlb_misses number = do
  r <- list_random_tlb number
  return (tlb_misses r)

-- Creates set of addresses, and a random victim, checks if the set is an eviction set for the victim
test_binary :: RepPol -> Int -> IO(Int)
test_binary pol number = do
  r <- random_SetOfAddresses number
  e <- evicts r pol
  return (bool_to_int e)

-- Creates set of addresses, and a random victim, checks if the set is an eviction set for the victim
test_assoc :: RepPol -> Int -> IO(Int)
test_assoc pol number = do
  let r = (SetState(16, 100)) 
  e <- evicts r pol
  return (bool_to_int e)
  
-- Mean of a list
mean :: [Int] -> Float
mean l = (fromIntegral $ sum l)/(fromIntegral $ length l)

bool_to_int :: Bool -> Int
bool_to_int True = 1
bool_to_int _ = 0


maybe_to_int :: Maybe(SetState) -> Int
maybe_to_int Nothing = 0
maybe_to_int _ = 1
