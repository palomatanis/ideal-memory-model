module Main where

import Base
import Cache_model
import Address_creation
import Algorithms
import ReplacementPolicies

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
numberCongAddresses_From = 6
-- at least 'dto'addresses

numberCongAddresses_To :: Int
numberCongAddresses_To = 32


rangeTests :: Int
rangeTests = 2 * associativity

iterations :: Int
iterations = 100

-- Eviction strategies
-- Range of values to test of C
cfrom = 1
cto = 3
-- Range of values to test of D
dfrom = 1
dto = 6
-- Range of values to test of L
lfrom = 1
lto = 6


-- eviction_strategies = filter (\(a, b, c) -> (b >= c) && ((a == 0) || (b == 0) ||(c == 0))) $ [ (x,y,z) | x<-[cfrom..cto], y<-[dfrom..dto], z<-[lfrom..lto] ]
-- eviction_strategies = filter (\(_, b, c) -> b >= c) $ [ (x,y,z) | x<-[cfrom..cto], y<-[dfrom..dto], z<-[lfrom..lto] ]
eviction_strategies = filter (\([(c1,_,_,n1,_),(c2,_,_,n2,_)], _) -> (c1 == c2) || (n1 == n2)) [ ([a,b], 1) | a <- es, b <- es ]
  where es = [ (c, 1, 1, n, 1) | c <- [cfrom..cto], n <- [numberCongAddresses_From..numberCongAddresses_To] ]

--policies = [(lru, "lru"), (plru, "plru"), (plru, "rplru"),(plru, "plrur"), (bip, "bip"), (lip, "lip"), (fifo, "fifo"), (mru, "mru"), (rr, "rr"), (srrip, "srrip"), (brrip, "brrip")]

policies = [(lru, "lru"), (bip, "bip"), (lip, "lip"), (rr, "rr"), (srrip, "srrip"), (brrip, "brrip")]
-- policies = [(plru, "plru")]

-- policies = [(srrip, "srrip")]

victim_position = [0]
--victim_position = [0..associativity-1]

m_srrip = [3]
-- msrrip = [1..16]
-- bip_probabilities = [16,18..62]                  
-- Calls the test for eviction with all the combinations of the eviction strategies


-- main = do
--   mapM executeV m_srrip
--   where
--     executeV d = do
--        mapM (\v -> mapM (execute d v) policies) victim_position
--     execute d v (pol, name) = do
--       mapM (\ev@(a,b,c) -> executeTestCongruent ("./adaptive/extra_patterns/congruent_adaptive_eviction_test_" ++ (show iterations) ++ "it_" ++ name ++"_count_" ++ (show a) ++ "_" ++ (show b) ++ "_" ++ (show c)) pol pol ev v d) eviction_strategies


main = do
  mapM executeV m_srrip
  where
    executeV d = do
       mapM (\v -> mapM (execute d v) policies) victim_position
    execute d v (pol, name) = do
          mapM (\ev@([(a1,b1,c1,n1,_), (a2,b2,c2,n2,_)], _) -> executeTestCongruentExtra ("./adaptive/extra_patterns/congruent_adaptive_eviction_test_" ++ (show iterations) ++ "it_" ++ name ++"_count_a_" ++ (show a1) ++ "_" ++ (show b1) ++ "_" ++ (show c1) ++ "" ++ (show n1) ++ "_b_" ++ (show a2) ++ "_" ++ (show b2) ++ "_" ++ (show c2) ++ "" ++ (show n2)) pol ev d) eviction_strategies

          
-- path :: String
-- path = "./adaptive/adaptive_eviction_test_50_lru_bip_1_4_4"

-- Save tests
-- Performs test of eviction for adaptive replacement policies, saves the averages of the results

congruence = True

-- rangeAddresses = [numberAddrToTest_From, (numberAddrToTest_From + rangeTests)..numberAddrToTest_To]
rangeAddresses = [numberCongAddresses_From..numberCongAddresses_To]


-- test = test_adaptive_eviction
test = test_adaptive_eviction_congruent

executeTestAdaptive path p1 p2 es v d = do
  m <- test_complete $ test p1 p2 es v d
  let (ev, hits, psel) = unzip3 m
  outh <- openFile path WriteMode
  mapM (hPutStrLn outh . show) ev
  hClose outh
  outh <- openFile (path ++ "_hits") WriteMode
  mapM (hPutStrLn outh . show) hits
  hClose outh
  if (not congruence) then do
    outh <- openFile (path ++ "_psel") WriteMode
    mapM (hPutStrLn outh . show) psel
    hClose outh
    else return()
      where
        test_complete test = do
          p <- mapM (do_test_of test) rangeAddresses
          return p  
        do_test_of f n = do
          res <- replicateM iterations $ f n
          let (es, cs) = unzip res
          let (_,_,_,_,hss, pss) = unzip6 cs
          let hs = map (\(Hit x) -> x) hss
          -- let (es, hs, pss) = unzip3 res
          return ((mean es, mean hs, mean pss))


executeTestCongruent path p1 _ es v d = do
  t <- executeTestAdaptive path p1 p1 es v d
  return t


executeTestCongruentExtra path p1 es d = do
  (ev, hits, _) <- do_test_of $ test_adaptive_eviction_congruent_extra p1 es d
  outh <- openFile path WriteMode
  hPutStrLn outh $ show ev
  hClose outh
  outh <- openFile (path ++ "_hits") WriteMode
  hPutStrLn outh $ show hits
  hClose outh
    where
      do_test_of f = do
        res <- replicateM iterations $ f
        let (es, cs) = unzip res
        let (_,_,_,_,hss, pss) = unzip6 cs
        let hs = map (\(Hit x) -> x) hss
        -- let (es, hs, pss) = unzip3 res
        return ((mean es, mean hs, mean pss))


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
test_adaptive_eviction :: RepPol -> RepPol -> EvictionStrategy -> Int -> Int -> Int -> IO((Int, CacheState))
test_adaptive_eviction pol1 pol2 es v d number = do
  r <- long_address_set number
  let fresh_cache_state = create_fresh_state pol1 pol2 (initialSet number v) 512
  (ev, cs) <- evicts_adapt r fresh_cache_state es d
  return ((bool_to_int ev, cs))

--- NOW THE VICTIM IS ALREADY INSIDE THE CACHE        
-- Creates set of addresses, and a random victim, checks if the set is an eviction set for the victim
test_adaptive_eviction_congruent :: RepPol -> RepPol -> EvictionStrategy -> Int -> Int -> Int -> IO((Int, CacheState))
test_adaptive_eviction_congruent pol1 _ es v d number = do
  let r = congruent_long_address_set number 1
  let fresh_cache_state = create_fresh_state pol1 pol1 (initialSetPLRU) 512
  (ev, cs) <- evicts_adapt_count r fresh_cache_state es d
  return ((associativity - ev, cs))
  
  
-- Creates a new state of the hole cache, from the set number of the victim, and the initial state of the victim cache set
create_fresh_state :: RepPol -> RepPol -> CacheSetContent -> Int -> CacheState
create_fresh_state p1 p2 victim init = (p1, p2, victim, map (\x -> (x, (initialSet 0 0)))(l0++l1), Hit 0, init)
  where l0 = [0, ((2^cacheSet)`div` num_regions)..possible_caches]
        l1 = map (+1) l0
        possible_caches = if noise then (2^cacheSet) else (2^free_cache_bits) -1


--- NOW THE VICTIM IS ALREADY INSIDE THE CACHE        
-- Creates set of addresses, and a random victim, checks if the set is an eviction set for the victim
test_adaptive_eviction_congruent_extra :: RepPol ->  EvictionStrategyExtra -> Int -> IO((Int, CacheState))
test_adaptive_eviction_congruent_extra pol1 es d = do
  let fresh_cache_state = create_fresh_state pol1 pol1 (initialSet d 0) 512
  trace <- generate_trace es
  (ev,cs) <- evicts_adapt_count_extra trace fresh_cache_state d
  -- (ev, cs) <- test_adaptive_eviction_count' (0, fresh_cache_state) es d
  return ((associativity - ev, cs))

generate_trace :: EvictionStrategyExtra -> IO(SetAddresses)
generate_trace e = do
  r <- generate_trace' e [] 1
  return (SetAddresses r)
  where
    generate_trace' :: EvictionStrategyExtra -> [LongAddress] -> Int -> IO([LongAddress])
    generate_trace' ([], rep) lis _ = do
      let trace = concat $ replicate rep lis
      return trace
    generate_trace' (((c,d,l,s,repL):es), repG) lis n = do
      let set = congruent_long_address_set s n
      (SetAddresses tr) <- eviction_strategy_trace set (c,d,l)
      let trace = concat $ replicate repL tr
      rr <- generate_trace' (es, repG) (trace ++ lis) (n+s)
      return rr
      
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
