module Cache_model  where

import System.Random
import System.Random.Shuffle

import Data.List
import Data.List.Split

import Data.Char  
import Numeric

import Data.Random
import Data.Maybe
--import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Base
import Address_creation


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
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            r <- lru'(Trace $ tail trace, CacheSetContent $ (0,h) : (deleteN elem set), Hit $ hit + 1)
            return r
          Nothing -> do
            r <- lru'(Trace $ tail trace, CacheSetContent $ (0,h) : init set, Hit hit)
            return r
        where h = head trace

 -- Least recently used
plru :: RepPol
plru set trace = do
  (t, s, h) <- plru'(trace, set, Hit 0)
  return (s, h)
    where
      plru' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
      plru' i@(Trace [], _, _) = do return i
      plru' (Trace trace, CacheSetContent set, Hit hit) =
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            new_set <- plru_find elem set
            r <- plru'(Trace $ tail trace, CacheSetContent new_set, Hit $ hit + 1)
            return r
          Nothing -> do
            (up_set, elem) <- plru_insert set
            let (reg,_) = up_set !! elem
            let new_set = take elem up_set ++ [(reg, h)] ++ drop (elem + 1) up_set
            r <- plru'(Trace $ tail trace, CacheSetContent new_set, Hit hit)
            return r
        where h = head trace

        
plru_insert :: [(Int, AddressIdentifier)] -> IO(([(Int, AddressIdentifier)], Int))
plru_insert set = plru_insert' (associativity `div` 2) set ""
  where
    plru_insert' :: Int -> [(Int, AddressIdentifier)] -> String -> IO(([(Int, AddressIdentifier)], Int))
    plru_insert' 0 set list = do
      let r = readBin list
      return (set, r)
    plru_insert' tier set l = do
      b <- randomRIO(0, 1)
      let bin = if (a == 2) then b else a
      if (bin == 0)
        then do
        (ret, r) <- plru_insert' (if (tier == 1) then 0 else tier `div` 2) (drop 1 set) (l++(show 0))
        return (((if (a == 2) then a else 1), val) : ret, r)
        else do
        (ret, r) <- plru_insert' (if (tier == 1) then 0 else tier `div` 2) (drop tier set) (l++(show 1))
        return (((if (a == 2) then a else 0), val):(take (tier - 1) $ tail set) ++ ret, r)
          where
            (a, val) = head set


plru_find :: Int -> [(Int, AddressIdentifier)] -> IO([(Int, AddressIdentifier)])
plru_find address set = plru_find' (associativity `div` 2) set (to_bin address)
  where
    plru_find' :: Int -> [(Int, AddressIdentifier)] -> [Int] -> IO([(Int, AddressIdentifier)])
    plru_find' 0 set list = do return set
    plru_find' tier set (l:ls) = do
      if (l == 0)
        then do
        ret <- plru_find' (if (tier == 1) then 0 else tier `div` 2) (drop 1 set) ls
        return (((if (a == 2) then a else 1), val) : ret)
        else do
        ret <- plru_find' (if (tier == 1) then 0 else tier `div` 2) (drop tier set) ls
        return (((if (a == 2) then a else 0), val):(take (tier - 1) $ tail set) ++ ret)
          where
            (a, val) = head set


to_bin :: Int -> [Int]
to_bin n = to_length_bin $ to_bin' n
  where
    to_bin' :: Int -> [Int]
    to_bin' 0 = []
    to_bin' n | n `mod` 2 == 1 = to_bin' (n `div` 2) ++ [1]
              | n `mod` 2 == 0 = to_bin' (n `div` 2) ++ [0]
    to_length_bin :: [Int] -> [Int]
    to_length_bin l = if ((length l) == (truncate $ logBase (fromIntegral 2) (fromIntegral associativity))) then l else to_length_bin (0:l)

    
readBin :: String -> Int
readBin = fromMaybe 0 . toBin
  where toBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

-- Most recently used
mru :: RepPol
mru set trace = do
  (t, s, h) <- mru'(trace, set, Hit 0)
  return (s, h)
    where
      mru' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
      mru' i@(Trace [], _, _) = do return i
      mru' (Trace trace, CacheSetContent set, Hit hit) =
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            r <- mru'(Trace (tail trace), CacheSetContent((deleteN elem set) ++ [(0,h)]), Hit (hit + 1))
            return r
          Nothing -> do
            r <- mru'(Trace (tail trace), CacheSetContent((init set) ++ [(0,h)]), Hit hit)
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
        case (elemIndex h $ map (\(a,b)-> b) set) of
          Just elem -> do
            r <- rr'(Trace (tail trace), CacheSetContent set, Hit (hit + 1))
            return r
          Nothing -> do
            b <- randomRIO(0, (associativity - 1))
            r <- rr' (Trace (tail trace), CacheSetContent ((0,h) : (deleteN b set)), Hit hit)
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
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just _ -> do
            r <- fifo'(Trace (tail trace), CacheSetContent set, Hit (hit + 1))
            return r
          Nothing -> do
            r <- fifo'(Trace (tail trace), CacheSetContent((0,h) : init set), Hit hit)
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
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            r <- lip'(Trace (tail trace), CacheSetContent (if (elem == ((length set) - 1)) then ((0,h) : (init set)) else set), Hit (hit + 1))
            return r
          Nothing -> do
            r <- lip'(Trace (tail trace), CacheSetContent (init set ++ [(0,h)]), Hit hit)
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
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            r <- bip'(Trace (tail trace), CacheSetContent (if (elem == ((length set) - 1)) then ((0,h) : (init set)) else set), Hit (hit + 1))
            return r
          Nothing -> do
            b <- chance64 2
            if b
              then bip'(Trace (tail trace), CacheSetContent ((0,h) : init set), Hit hit)
              else bip'(Trace (tail trace), CacheSetContent (init set ++ [(0,h)]), Hit hit)
        where h = head trace

-- Adaptive replacement policy
type AdaptiveRepPol = SetAddresses -> IO(CacheSetContent, HitNumber)



 -- Static re-reference interval prediction

-- Hit promotion policy = hp hit priority or fp frequency priority
hp = True -- if hp is False, then fp

srrip :: RepPol
srrip set trace = do
  (t, s, h) <- srrip'(trace, set, Hit 0)
  return (s, h)
    where
      srrip' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
      srrip' i@(Trace [], _, _) = do return i
      srrip' (Trace trace, CacheSetContent set, Hit hit) =
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            let (reg, el) = set !! elem
            let new_cache_st = take elem set ++ [((if hp then 0 else (if reg > 0 then reg-1 else 0)), h)] ++ drop (elem + 1) set
            r <- srrip'(Trace (tail trace), CacheSetContent(new_cache_st), Hit (hit + 1))
            return r
          Nothing ->
            case (elemIndex (2^m-1) $ map (\(a,b) -> a) set) of
              Just elem -> do -- There is a rrpv register with a (2^m-1)
                let (reg, el) = set !! elem
                let new_cache_st = take elem set ++ [(2^m-2, h)] ++ drop (elem + 1) set
                r <- srrip'(Trace (tail trace), CacheSetContent(new_cache_st), Hit hit)
                return r
              Nothing -> do
                let new_cache_st = map (\(a,b) -> (a+1,b)) set
                r <- srrip'(Trace trace, CacheSetContent(new_cache_st), Hit hit)
                return r
        where h = head trace

-- Bimodal RRIP - puts new blocks with a distant rrpv (2^m-1) most of the time, but sometimes in long rrpv (2^m-2)
brrip :: RepPol
brrip set trace = do
  (t, s, h) <- brrip'(trace, set, Hit 0)
  return (s, h)
    where
      brrip' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
      brrip' i@(Trace [], _, _) = do return i
      brrip' (Trace trace, CacheSetContent set, Hit hit) =
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            let (reg, el) = set !! elem
            let new_cache_st = take elem set ++ [((if hp then 0 else (if reg > 0 then reg-1 else 0)), el)] ++ drop (elem + 1) set
            r <- brrip'(Trace (tail trace), CacheSetContent(new_cache_st), Hit (hit + 1))
            return r
          Nothing ->
            case (elemIndex (2^m-1) $ map (\(a,b) -> a) set) of
              Just elem -> do -- There is a rrpv register with a (2^m-1)
                let (reg, el) = set !! elem
                b <- chance64 2
                let new_cache_st = if b
                      then (take elem set ++ [(2^m-2, h)] ++ drop (elem + 1) set)
                      else (take elem set ++ [(2^m-1, h)] ++ drop (elem + 1) set)
                r <- brrip'(Trace (tail trace), CacheSetContent(new_cache_st), Hit hit)
                return r
              Nothing -> do
                let new_cache_st = map (\(a,b) -> (a+1,b)) set
                r <- brrip'(Trace trace, CacheSetContent(new_cache_st), Hit hit)
                return r
        where h = head trace
                   
-- Calls the replacement policy with/without noise
adaptiveCacheInsert :: SetAddresses -> CacheState -> IO(CacheState)
adaptiveCacheInsert (SetAddresses addresses) fresh_state = do
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

-- Is the one that inserts all the addresses, the ones in the victim addresses on one side and leaders on the other
adaptivePolicy :: SetAddresses -> StateT CacheState IO (CacheState)
adaptivePolicy (SetAddresses []) = do
  cs@(p1, p2, csc, _, h, psel) <- get
  return cs
adaptivePolicy (SetAddresses ((LongAddress(id, Address x)):xs)) = do
  (p1, p2, csc, l, h, psel) <- get
  if (x == targetSet)
    then callPol id
    else callLeader id x (x `mod` ((2^cacheSet) `div` num_regions))
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
