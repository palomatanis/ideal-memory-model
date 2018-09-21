module Cache_model  where

import System.Random
import System.Random.Shuffle

import Data.List
import Data.List.Split

import Data.Random

--import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Base
import Address_creation


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
    where
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
    where
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

-- Random replacement
rr :: RepPol
rr set trace = do
  (t, s, h) <- rr'(trace, set, Hit 0)
  return (s, h)
    where
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
    where
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
    where
      lip' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
      lip' i@(Trace [], _, _) = do return i
      lip' (Trace trace, CacheSetContent set, Hit hit) = 
        case (elemIndex h set) of
          Just elem -> do
            r <- lip'(Trace (tail trace), CacheSetContent (if (elem == ((length set) - 1)) then (h : (init set)) else set), Hit (hit + 1))
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
    where
      bip' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
      bip' i@(Trace [], _, _) = do return i
      bip' (Trace trace, CacheSetContent set, Hit hit) = 
        case (elemIndex h set) of
          Just elem -> do
            r <- bip'(Trace (tail trace), CacheSetContent (if (elem == ((length set) - 1)) then (h : (init set)) else set), Hit (hit + 1))
            return r
          Nothing -> do
            b <- chance64 2
            if b
              then bip'(Trace (tail trace), CacheSetContent (h : init set), Hit hit)
              else bip'(Trace (tail trace), CacheSetContent (init set ++ [h]), Hit hit)
        where h = head trace

-- Adaptive replacement policy
type AdaptiveRepPol = SetAddresses -> IO(CacheSetContent, HitNumber)

-- Calls the replacement policy with/without noise
adaptiveCacheInsert :: RepPol -> RepPol -> CacheSetContent -> SetAddresses -> IO(CacheSetContent, HitNumber, Int)
adaptiveCacheInsert pol1 pol2 setcontent (SetAddresses addresses) = do
  let fresh_state = create_fresh_state pol1 pol2 setcontent 512
--  let fresh_state = create_fresh_state pol1 pol2 setcontent 512
  case noise of
    True -> do
      -- The TLB misses should not be at the end
      (SetAddresses t) <- tlb_set (expected_tlb_misses $ length addresses) $ length addresses
      let new_trace = SetAddresses (addresses ++ t)
      r <- evalStateT (adaptivePolicy new_trace) fresh_state
      return r
    False -> do
      r <- evalStateT (adaptivePolicy (SetAddresses addresses)) fresh_state
      return r

-- Creates a new state of the hole cache, from the set number of the victim, and the initial state of the victim cache set
create_fresh_state :: RepPol -> RepPol -> CacheSetContent -> Int -> CacheState
create_fresh_state p1 p2 victim init = (p1, p2, victim, map (\x -> (x, initialSet))(l0++l1), Hit 0, init)
  where l0 = [0, ((2^cacheSet)`div` num_regions)..possible_caches]
        l1 = map (+1) l0
        possible_caches = if noise then (2^cacheSet) else (2^free_cache_bits) -1

-- Is the one that inserts all the addresses, the ones in the victim addresses on one side and leaders on the other
adaptivePolicy :: SetAddresses -> StateT CacheState IO (CacheSetContent, HitNumber, Int)
adaptivePolicy (SetAddresses []) = do
  (p1, p2, csc, _, h, psel) <- get
  return (csc, h, psel)
adaptivePolicy (SetAddresses (x:xs)) = do
  (p1, p2, csc, l, h, psel) <- get
  case x of
    LongAddress (id, Address 2) -> callPol id
    LongAddress (id, Address n) -> callLeader id n (n `mod` ((2^cacheSet) `div` num_regions))
  adaptivePolicy (SetAddresses xs)

-- Calls the corresponding replacement policy for a victim address, as psel says. Updates the content of the victim, and number of hits
-- (new_csc, new_hn, new_psel) <- callPol csc id psel    
callPol :: AddressIdentifier -> StateT CacheState IO ()
callPol id = do
  (p1, p2, csc, l, Hit h, psel) <- get
  let p = if (psel > 512) then p2 else p1
  (new_cacheContent, Hit hit) <- liftIO $ p csc (Trace [id])
  put (p1, p2, new_cacheContent, l, Hit (h+hit), psel)

-- Calls replacement policy for a leader address, updates psel and state of the whole cache 
-- (new_l, new_psel) <- callLeader id n l psel 0/1
callLeader :: AddressIdentifier -> Int -> Int -> StateT CacheState IO ()
callLeader id n mod = do
  (p1, p2, csc, l, h, psel) <- get
  if ((mod == 0) || (mod == 1)) then do
    let (a, rest) = take_from_list (\(x, _) -> x == n) l
    case a of
      Just (n, cacheContent) -> do
        let p = if (mod == 0) then p1 else p2
        (new_cacheContent, Hit hit) <- liftIO $ p cacheContent $ Trace [id]
        let new_psel = saturating_psel $ psel + (if (mod == 0) then (1-hit) else (-1+hit))
        let new_cachestate = (p1, p2, csc, ((n, new_cacheContent): rest), h, new_psel)
        put (new_cachestate)
      _ -> put (p1, p2, csc, l, h, psel)
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
