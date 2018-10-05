module Cache_model  where


import Data.List.Split
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Base
import Address_creation
import ReplacementPolicies

import Control.Concurrent

---- TLB
-- TLB misses for a set of addresses
tlb_misses :: [Int] -> Int
tlb_misses = sum . map (\x -> x - tlb_block_size) . filter (> tlb_block_size) . tlb_misses'
  where
    tlb_misses' tlbs = map (\x -> length $ filter (== x) tlbs) [0..((2^tlb_bits) - 1)]
    
tlb_block_size :: Int
tlb_block_size = truncate $ (fromIntegral tlb_size) / (fromIntegral $ 2^tlb_bits)


---- REPLACEMENT POLICIES

noise = False

-- Calls the replacement policy with/without noise
cacheInsert :: RepPol -> CacheSetContent -> Trace -> Int -> IO(CacheSetContent, HitNumber)
cacheInsert policy set tr@(Trace t) total = do
  case noise of
    True -> do
      ev <- tlb_congruent $ expected_tlb_misses total
      let n = length t
      let new_trace = if (ev == 0) then tr else (Trace (t ++ (map AddressIdentifier [(n + 1)..(n + ev)])))
      r <- policy set new_trace 0
      return r
    False -> do
      r <- policy set tr 0
      return r
  
-- Expected number of misses for a number of addresses
expected_tlb_misses :: Int -> Int
expected_tlb_misses n =
  let d = n - tlb_size
  in if d > 0 then d else 0
                   
-- Calls the replacement policy with/without noise
adaptiveCacheInsert :: SetAddresses -> CacheState -> Int -> IO(CacheState)
adaptiveCacheInsert (SetAddresses addresses) fresh_state d = do
  case noise of
    True -> do
      -- The TLB misses should not be at the end
      (SetAddresses t) <- tlb_set (expected_tlb_misses $ length addresses) $ length addresses
      let new_trace = SetAddresses (addresses ++ t)
      r <- evalStateT (adaptivePolicy new_trace d) fresh_state
      return r
    False -> do
      r <- evalStateT (adaptivePolicy (SetAddresses addresses) d) fresh_state
      return r

-- Is the one that inserts all the addresses, the ones in the victim addresses on one side and leaders on the other
adaptivePolicy :: SetAddresses -> Int -> StateT CacheState IO (CacheState)
adaptivePolicy (SetAddresses []) _ = do
  cs@(p1, p2, csc, _, h, psel) <- get
  return cs
adaptivePolicy (SetAddresses ((LongAddress(id, Address x)):xs)) d = do
  (p1, p2, csc, l, h, psel) <- get
  if (x == targetSet)
    then callPol id d
    else callLeader id x (x `mod` ((2^cacheSet) `div` num_regions)) d
  adaptivePolicy (SetAddresses xs) d

-- Calls the corresponding replacement policy for a victim address, as psel says. Updates the content of the victim, and number of hits
-- (new_csc, new_hn, new_psel) <- callPol csc id psel    
callPol :: AddressIdentifier -> Int -> StateT CacheState IO ()
callPol id d = do
  (p1, p2, csc, l, Hit h, psel) <- get
  let p = if (psel > 512) then p2 else p1
  (new_cacheContent, Hit hit) <- liftIO $ p csc (Trace [id]) d
  put (p1, p2, new_cacheContent, l, Hit (h+hit), psel)

-- Calls replacement policy for a leader address, updates psel and state of the whole cache 
-- (new_l, new_psel) <- callLeader id n l psel 0/1
callLeader :: AddressIdentifier -> Int -> Int -> Int -> StateT CacheState IO ()
callLeader id n mod d = do
  (p1, p2, csc, l, h, psel) <- get
  if ((mod == 0) || (mod == 1)) then do
    let (a, rest) = take_from_list (\(x, _) -> x == n) l
    case a of
      Just (n, cacheContent) -> do
        let p = if (mod == 0) then p1 else p2
        (new_cacheContent, Hit hit) <- liftIO $ p cacheContent (Trace [id]) d
        let new_psel = saturating_psel $ psel + (if (mod == 0) then (1-hit) else (-1+hit))
        let new_cachestate = (p1, p2, csc, ((n, new_cacheContent): rest), h, new_psel)
        put (new_cachestate)
  else put (p1, p2, csc, l, h, psel)


---- Aux
saturating_psel :: Int -> Int
saturating_psel n
  | n > 1024 = 1024
  | n < 0 = 0
  | otherwise = n

-- Takes list of sets and outputs the histogram
separate_sets_into_bins :: [Address] -> [Int]
separate_sets_into_bins sets = map (\x -> (length $ filter (==x) $ map show_set sets)) $ [0..(free_cache - 1)]

-- Takes list and predicate and returns elements that satisfies predicate and rest of List
take_from_list :: (a -> Bool) -> [a] -> (Maybe a, [a])
take_from_list f l = take_from_list' f l []

take_from_list' :: (a -> Bool) -> [a] -> [a] -> (Maybe a, [a])
take_from_list' f [] l = (Nothing, l)
take_from_list' f (x:xs) l =
  if (f(x)) then (Just x, xs ++ l)
  else take_from_list' f xs (x:l)
