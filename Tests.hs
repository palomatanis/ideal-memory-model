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

  
numberCongAddresses_From :: Int
numberCongAddresses_From = 10

numberCongAddresses_To :: Int
numberCongAddresses_To = 30


rangeTests :: Int
rangeTests = 2 * associativity

iterations :: Int
iterations = 50

-- Eviction strategies
-- Range of values to test of C
cfrom = 1 
cto = 6
-- Range of values to test of D
dfrom = 1
dto = 6
-- Range of values to test of L
lfrom = 1
lto = 6

-- Calls the test for eviction with all the combinations of the eviction strategies
main = do
  let eviction_strategies = filter (\(_, b, c) -> b >= c) $ [ (x,y,z) | x<-[cfrom..cto], y<-[dfrom..dto], z<-[lfrom..lto] ]
  mapM (\ev@(a, b, c) -> executeTestAdaptive ("./adaptive/adaptive_eviction_test_50it_0psel_lru_bip_" ++ (show a) ++ "_" ++ (show b) ++ "_" ++ (show c)) lru bip ev) eviction_strategies

-- path :: String
-- path = "./adaptive/adaptive_eviction_test_50_lru_bip_1_4_4"

-- Save tests
-- Performs test of eviction for adaptive replacement policies, saves the averages of the results
executeTestAdaptive path p1 p2 es = do
  m <- test_complete $ test_adaptive_eviction p1 p2 es
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

executeTestCongruent path p1 es = do
  m <- test_complete $ test_adaptive_eviction p1 p1 es
  let (ev, hits, _) = unzip3 m
  outh <- openFile path WriteMode
  mapM (hPutStrLn outh . show) ev
  hClose outh
  outh <- openFile (path ++ "_hits") WriteMode
  mapM (hPutStrLn outh . show) hits
  hClose outh
    where
      test_complete test = do
        p <- mapM (do_test_of test) [numberCongAddresses_From..numberCongAddresses_To]
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

-- Execute all tests and save results to file
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
test_adaptive_eviction_congruent :: RepPol ->  EvictionStrategy -> Int -> IO((Int, Int, Int))
test_adaptive_eviction_congruent pol1 es number = do
  let r = congruent_long_address_set number
  (ev, hits, psel) <- evicts_adapt r pol1 pol1 es
  return ((bool_to_int ev, hits, psel))

  
-- Creates set of addresses, and a random victim, checks if the set is an eviction set for the victim
test_assoc :: RepPol -> Int -> IO(Int)
test_assoc pol number = do
  let r = (SetState(16, 100)) 
  e <- evicts r pol
  return (bool_to_int e)


-- Aux

-- Mean of a list
mean :: [Int] -> Float
mean l = (fromIntegral $ sum l)/(fromIntegral $ length l)

bool_to_int :: Bool -> Int
bool_to_int True = 1
bool_to_int _ = 0

maybe_to_int :: Maybe(SetState) -> Int
maybe_to_int Nothing = 0
maybe_to_int _ = 1
