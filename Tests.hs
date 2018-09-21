module Tests where

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
iterations = 50

-- path :: String
-- path = "./adaptive/adaptive_eviction_test_50_lru_bip_1_4_4"

-- Save tests

executeTestAdaptive path p1 p2 es = do
  m@[(a, b, c)] <- test_complete $ test_adaptive_eviction p1 p2 es
  let (ev, hits, psel) = unzip3 m
  outh <- openFile path WriteMode
  mapM (hPutStrLn outh . show) ev
  hClose outh
  outh <- openFile (path ++ "_hits") WriteMode
  mapM (hPutStrLn outh . show) hits
  hClose outh
  outh <- openFile (path ++ "_psel") WriteMode
  mapM (hPutStrLn outh . show) psel
  hClose outh
    where
      test_complete test = do
        p <- mapM (do_test_of test) [numberAddrToTest_From, (numberAddrToTest_From + rangeTests)..numberAddrToTest_To]
        return p  
      do_test_of f n = do
        res <- replicateM iterations $ f n
        let (es, hs, pss) = unzip3 res
        return ((mean es, mean hs, mean pss))


-- executeTestAdaptive path p1 p2 es = do
--   m@[(a, b, c)] <- test_complete $ test_adaptive_eviction p1 p2 es
--   let (ev, hits, psel) = unzip3 m
--   writeFile path ((unwords $ map show ev) ++ "\n")
--     where
--       test_complete test = do
--         p <- mapM (do_test_of test) [numberAddrToTest_From, (numberAddrToTest_From + rangeTests)..numberAddrToTest_To]
--         return p  
--       do_test_of f n = do
--         res <- replicateM iterations $ f n
--         let (es, hs, pss) = unzip3 res
--         return ((mean es, mean hs, mean pss))

        
executeTestGeneric path p1 = do
  m <- test_complete $ test_eviction p1
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
test_eviction :: RepPol -> Int -> IO(Int)
test_eviction pol number = do
  r <- random_SetOfAddresses number
  e <- evicts r pol
  return (bool_to_int e)

-- Creates set of addresses, and a random victim, checks if the set is an eviction set for the victim
test_adaptive_eviction :: RepPol -> RepPol -> EvictionStrategy -> Int -> IO((Int, Int, Int))
test_adaptive_eviction pol1 pol2 es number = do
  r <- long_address_set number
  (ev, hits, psel) <- evicts_adapt r pol1 pol2 es
  return ((bool_to_int ev, hits, psel))


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
