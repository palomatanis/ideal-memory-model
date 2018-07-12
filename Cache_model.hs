module Cache_model  where

import System.Random
import System.Random.Shuffle

import Data.List
import Data.List.Split

import Data.Random

import Base
import Address_creation


-- ---- Binary test

-- -- Is there at least one eviction set
-- exists_eviction :: [Address] -> Bool
-- exists_eviction add = (number_of_eviction_sets add) > 0

-- -- Count number of addresses in eviction sets
-- number_of_eviction_addresses :: [Address] -> Int
-- number_of_eviction_addresses = sum . filter (> associativity) . separate_sets_into_bins

-- -- Returns number of eviction sets for a list of sets
-- number_of_eviction_sets :: [Address] -> Int
-- number_of_eviction_sets = length . filter (> associativity) . separate_sets_into_bins



---- TLB
-- TLB misses for a set of addresses
tlb_misses :: [Int] -> Int
tlb_misses = sum . map (\x -> x - tlb_block_size) . filter (> tlb_block_size) . tlb_misses'
  where
    tlb_misses' tlbs = map (\x -> length $ filter (== x) tlbs) [0..((2^tlb_bits) - 1)]
    
tlb_block_size :: Int
tlb_block_size = truncate $ (fromIntegral tlb_size) / (fromIntegral $ 2^tlb_bits)


---- REPLACEMENT POLICIES
type RepPol = CacheSetContent -> Trace -> IO(CacheSetContent, HitNumber)

noise = False

-- Calls the replacement policy with/without noise
cacheInsert :: RepPol -> CacheSetContent -> Trace -> Int -> IO(CacheSetContent, HitNumber)
cacheInsert policy set tr@(Trace t) total = do
  case noise of
    True -> do
      ev <- tlb_congruent $ expected_tlb_misses total
      let n = length t
      let new_trace = if (ev == 0) then tr else (Trace (t ++ (map AddressIdentifier [(n + 1)..(n + ev)])))
      r <- policy set new_trace
      return r
    False -> do
      r <- policy set tr
      return r
  
-- Expected number of misses for a number of addresses
expected_tlb_misses :: Int -> Int
expected_tlb_misses n =
  let d = n - tlb_size
  in if d > 0 then d else 0
  
 -- Least recently used
lru :: RepPol
lru set trace = do
  (t, s, h) <- lru'(trace, set, Hit 0)
  return (s, h)
  
lru' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
lru' i@(Trace [], _, _) = do return i
lru' (Trace trace, CacheSetContent set, Hit hit) =
  case (elemIndex h set) of
    Just elem -> do
      r <- lru'(Trace (tail trace), CacheSetContent(h : (deleteN elem set)), Hit (hit + 1))
      return r
    Nothing -> do
      r <- lru'(Trace (tail trace), CacheSetContent(h : init set), Hit hit)
      return r
  where h = head trace


-- Most recently used
mru :: RepPol
mru set trace = do
  (t, s, h) <- mru'(trace, set, Hit 0)
  return (s, h)

mru' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
mru' i@(Trace [], _, _) = do return i
mru' (Trace trace, CacheSetContent set, Hit hit) =
  case (elemIndex h set) of
    Just elem -> do
      r <- mru'(Trace (tail trace), CacheSetContent((deleteN elem set) ++ [h]), Hit (hit + 1))
      return r
    Nothing -> do
      r <- mru'(Trace (tail trace), CacheSetContent((init set) ++ [h]), Hit hit)
      return r
  where h = head trace

-- mru' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
-- mru' i@(Trace [], _, _) = do return i
-- mru' (Trace trace, CacheSetContent set, Hit hit) =
--   case (elemIndex h set) of
--     Just elem -> do
--       r <- mru'(Trace (tail trace), CacheSetContent((deleteN elem set) ++ [h]), Hit (hit + 1))
--       return r
--     Nothing ->
--       case (elemIndex (AddressIdentifier 0) set) of
--         Just elem -> do
--           r <- mru'(Trace (tail trace), CacheSetContent((deleteN elem set) ++ [h]), Hit hit)
--           return r
--         Nothing -> do
--           r <- mru'(Trace (tail trace), CacheSetContent((init set) ++ [h]), Hit hit)
--           return r
--   where h = head trace

-- Random replacement
rr :: RepPol
rr set trace = do
  (t, s, h) <- rr'(trace, set, Hit 0)
  return (s, h)

rr' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
rr' i@(Trace [], _, _) = do return i
rr' (Trace trace, CacheSetContent set, Hit hit) = 
  case (elemIndex h set) of
    Just elem -> do
      r <- rr'(Trace (tail trace), CacheSetContent set, Hit (hit + 1))
      return r
    Nothing ->
      case (elemIndex (AddressIdentifier 0) set) of
        Just elem -> do
          r <- rr'(Trace (tail trace), CacheSetContent(h : (deleteN elem set)), Hit hit)
          return r
        Nothing -> do
          b <- randomRIO(0, (associativity - 1))
          r <- rr' (Trace (tail trace), CacheSetContent (h : (deleteN b set)), Hit hit)
          return r
  where h = head trace


-- First in first out
fifo :: RepPol
fifo set trace = do
  (t, s, h) <- fifo'(trace, set, Hit 0)
  return (s, h)

fifo' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
fifo' i@(Trace [], _, _) = do return i
fifo' (Trace trace, CacheSetContent set, Hit hit) =
  case (elemIndex h set) of
    Just _ -> do
      r <- fifo'(Trace (tail trace), CacheSetContent set, Hit (hit + 1))
      return r
    Nothing -> do
      r <- fifo'(Trace (tail trace), CacheSetContent(h : init set), Hit hit)
      return r
  where h = head trace


-- -- LRU insertion policy
lip :: RepPol
lip set trace = do
  (t, s, h) <- lip'(trace, set, Hit 0)
  return (s, h)

lip' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
lip' i@(Trace [], _, _) = do return i
lip' (Trace trace, CacheSetContent set, Hit hit) = 
  case (elemIndex h set) of
    Just elem -> do
      r <- lip'(Trace (tail trace), CacheSetContent (if (elem == ((length set) - 1)) then (h : (init set)) else set), Hit (hit + 1))
      return r
    Nothing ->
      case (elemIndex (AddressIdentifier 0) set) of
        Just elem -> do
          r <- lip'(Trace (tail trace), CacheSetContent(h : (deleteN elem set)), Hit hit)
          return r
        Nothing -> do
          r <- lip'(Trace (tail trace), CacheSetContent (init set ++ [h]), Hit hit)
          return r
  where h = head trace
  

-- Bimodal insertion policy
bip :: RepPol
bip set trace = do
  (t, s, h) <- bip'(trace, set, Hit 0)
  return (s, h)

bip' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
bip' i@(Trace [], _, _) = do return i
bip' (Trace trace, CacheSetContent set, Hit hit) = 
  case (elemIndex h set) of
    Just elem -> do
      r <- bip'(Trace (tail trace), CacheSetContent (if (elem == ((length set) - 1)) then (h : (init set)) else set), Hit (hit + 1))
      return r
    Nothing ->
      case (elemIndex (AddressIdentifier 0) set) of
        Just elem -> do
          r <- bip'(Trace (tail trace), CacheSetContent(h : (deleteN elem set)), Hit hit)
          return r
        Nothing -> do
          b <- chance64 2
          if b
            then bip'(Trace (tail trace), CacheSetContent (h : init set), Hit hit)
            else bip'(Trace (tail trace), CacheSetContent (init set ++ [h]), Hit hit)
  where h = head trace


---- Aux

-- Takes list of sets and outputs the histogram
separate_sets_into_bins :: [Address] -> [Int]
separate_sets_into_bins sets = map (\x -> (length $ filter (==x) $ map show_set sets)) $ [0..(free_cache - 1)]
