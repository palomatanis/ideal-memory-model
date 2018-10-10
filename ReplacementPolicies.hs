module ReplacementPolicies where

import Base
import Address_creation

import Data.List

import Data.Char
import Numeric
-- import Data.Random
import Data.Maybe

import System.Random

 -- Least recently used
lru :: RepPol
lru set trace _ = do
  (t, s, h) <- lru'(trace, set, Hit 0)
  return (s, h)
    where
      lru' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
      lru' i@(Trace [], _, _) = do return i
      lru' (Trace trace, CacheSetContent set, Hit hit) =
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            let (reg,_) = set !! elem
            lru'(Trace $ tail trace, CacheSetContent $ (reg,h) : (deleteN elem set), Hit $ hit + 1)
          Nothing -> do
            let (reg,_) = last set
            lru'(Trace $ tail trace, CacheSetContent $ (reg,h) : init set, Hit hit)
        where h = head trace

 -- Least recently used
plru :: RepPol
plru set trace _ = do
  (t, s, h) <- plru'(trace, set, Hit 0)
  return (s, h)
    where
      plru' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
      plru' i@(Trace [], _, _) = do return i
      plru' (Trace trace, CacheSetContent set, Hit hit) =
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            new_set <- plru_find elem set
            plru'(Trace $ tail trace, CacheSetContent new_set, Hit $ hit + 1)
          Nothing -> do
            (up_set, elem) <- plru_insert set
            let (reg,_) = up_set !! elem
            let new_set = take elem up_set ++ [(reg, h)] ++ drop (elem + 1) up_set
            plru'(Trace $ tail trace, CacheSetContent new_set, Hit hit)
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
mru set trace _ = do
  (t, s, h) <- mru'(trace, set, Hit 0)
  return (s, h)
    where
      mru' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
      mru' i@(Trace [], _, _) = do return i
      mru' (Trace trace, CacheSetContent set, Hit hit) =
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            let (reg, _) = set !! elem
            mru'(Trace (tail trace), CacheSetContent((deleteN elem set) ++ [(reg,h)]), Hit (hit + 1))
          Nothing -> do
            let (reg,_) = last set
            mru'(Trace (tail trace), CacheSetContent((init set) ++ [(reg,h)]), Hit hit)
        where h = head trace

-- Random replacement
rr :: RepPol
rr set trace _ = do
  (t, s, h) <- rr'(trace, set, Hit 0)
  return (s, h)
    where
      rr' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
      rr' i@(Trace [], _, _) = do return i
      rr' (Trace trace, CacheSetContent set, Hit hit) = 
        case (elemIndex h $ map (\(a,b)-> b) set) of
          Just elem -> do
            rr'(Trace (tail trace), CacheSetContent set, Hit (hit + 1))
          Nothing -> do
            b <- randomRIO(0, (associativity - 1))
            let (reg, _) = set !! b
            let newSet = (take b set) ++ [(reg, h)] ++ drop (b + 1) set
            rr' (Trace (tail trace), CacheSetContent (newSet), Hit hit)
        where h = head trace

-- First in first out
fifo :: RepPol
fifo set trace _ = do
  (t, s, h) <- fifo'(trace, set, Hit 0)
  return (s, h)
    where
      fifo' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
      fifo' i@(Trace [], _, _) = do return i
      fifo' (Trace trace, CacheSetContent set, Hit hit) =
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just _ -> do
            fifo'(Trace (tail trace), CacheSetContent set, Hit (hit + 1))
          Nothing -> do
            fifo'(Trace (tail trace), CacheSetContent((0,h) : init set), Hit hit)
        where h = head trace

-- -- LRU insertion policy
lip :: RepPol
lip set trace _ = do
  (t, s, h) <- lip'(trace, set, Hit 0)
  return (s, h)
    where
      lip' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
      lip' i@(Trace [], _, _) = do return i
      lip' (Trace trace, CacheSetContent set, Hit hit) = 
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            lip'(Trace (tail trace), CacheSetContent (if (elem == ((length set) - 1)) then ((0,h) : (init set)) else set), Hit (hit + 1))
          Nothing -> do
            lip'(Trace (tail trace), CacheSetContent (init set ++ [(0,h)]), Hit hit)
        where h = head trace

-- Bimodal insertion policy
bip :: RepPol
bip set trace bip_probability = do
  (t, s, h) <- bip'(trace, set, Hit 0)
  return (s, h)
    where
      bip' :: (Trace, CacheSetContent, HitNumber) -> IO(Trace, CacheSetContent, HitNumber)
      bip' i@(Trace [], _, _) = do return i
      bip' (Trace trace, CacheSetContent set, Hit hit) = 
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            let (reg,_) = set !! elem
            r <- bip'(Trace (tail trace), CacheSetContent (if (elem == ((length set) - 1)) then ((reg,h) : (init set)) else set), Hit (hit + 1))
            return r
          Nothing -> do
            b <- chance64 bip_probability
            if b
              then do
              let (reg,_) = head set
              bip'(Trace (tail trace), CacheSetContent ((reg,h) : init set), Hit hit)
              else do
              let (reg,_) = last set
              bip'(Trace (tail trace), CacheSetContent (init set ++ [(reg,h)]), Hit hit)
        where h = head trace

-- Adaptive replacement policy
type AdaptiveRepPol = SetAddresses -> IO(CacheSetContent, HitNumber)



 -- Static re-reference interval prediction

-- Hit promotion policy = hp hit priority or fp frequency priority
hp = True -- if hp is False, then fp

srrip :: RepPol
srrip set trace m = do
  (t, s, h) <- srrip'(trace, set, Hit 0) m
  return (s, h)
    where
      srrip' :: (Trace, CacheSetContent, HitNumber) -> Int -> IO(Trace, CacheSetContent, HitNumber)
      srrip' i@(Trace [], _, _) _  = do return i
      srrip' (Trace trace, CacheSetContent set, Hit hit) m =
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            let (reg, el) = set !! elem
            let new_cache_st = take elem set ++ [((if hp then 0 else (if reg > 0 then reg-1 else 0)), h)] ++ drop (elem + 1) set
            srrip'(Trace (tail trace), CacheSetContent(new_cache_st), Hit (hit + 1)) m
          Nothing ->
            case (elemIndex (2^m-1) $ map (\(a,b) -> a) set) of
              Just elem -> do -- There is a rrpv register with a (2^m-1)
                let (reg, el) = set !! elem
                let new_cache_st = take elem set ++ [(2^m-2, h)] ++ drop (elem + 1) set
                srrip'(Trace (tail trace), CacheSetContent(new_cache_st), Hit hit) m
              Nothing -> do
                let new_cache_st = map (\(a,b) -> (a+1,b)) set
                srrip'(Trace trace, CacheSetContent(new_cache_st), Hit hit) m
        where h = head trace

        
mbrrip = 3 :: Int
-- Bimodal RRIP - puts new blocks with a distant rrpv (2^m-1) most of the time, but sometimes in long rrpv (2^m-2)
brrip :: RepPol
brrip set trace bip_probability = do
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
            brrip'(Trace (tail trace), CacheSetContent(new_cache_st), Hit (hit + 1))
          Nothing ->
            case (elemIndex (2^mbrrip-1) $ map (\(a,b) -> a) set) of
              Just elem -> do -- There is a rrpv register with a (2^m-1)
                let (reg, el) = set !! elem
                b <- chance64 bip_probability
                let new_cache_st = if b
                      then (take elem set ++ [(2^mbrrip-2, h)] ++ drop (elem + 1) set)
                      else (take elem set ++ [(2^mbrrip-1, h)] ++ drop (elem + 1) set)
                brrip'(Trace (tail trace), CacheSetContent(new_cache_st), Hit hit)
              Nothing -> do
                let new_cache_st = map (\(a,b) -> (a+1,b)) set
                brrip'(Trace trace, CacheSetContent(new_cache_st), Hit hit)
        where h = head trace
