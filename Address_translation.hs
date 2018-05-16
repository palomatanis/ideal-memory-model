module Address_translation  where

import System.Random
import System.Random.Shuffle

import Data.List
import Data.List.Split

import Data.Random

import Address_creation
import ReplacementPolicies
import TestReplacementPolicies

---- Binary test

evicts :: CacheState -> RepPol -> IO(Bool)
evicts (CacheState (ev, _)) policy = do
  h <- testEvictionM policy ev
  let v = h == 1
  return v

-- Is there at least one eviction set
exists_eviction :: [Address] -> Bool
exists_eviction add = (number_of_eviction_sets add) > 0

-- Count number of addresses in eviction sets
number_of_eviction_addresses :: [Address] -> Int
number_of_eviction_addresses = sum . filter (> associativity) . separate_sets_into_bins

-- Returns number of eviction sets for a list of sets
number_of_eviction_sets :: [Address] -> Int
number_of_eviction_sets = length . filter (> associativity) . separate_sets_into_bins

-- Takes list of sets and outputs the histogram
separate_sets_into_bins :: [Address] -> [Int]
separate_sets_into_bins sets = map (\x -> (length $ filter (==x) $ map show_set sets)) $ [0..(free_cache - 1)]


-- Naive reduction for probabilistic replacement policies
naive_reduction :: CacheState ->  RepPol -> IO(Bool)
naive_reduction state@(CacheState(ev, total)) repPol = do
  set <- shuffleM $ (take ev $ repeat 1) ++ (take (total - ev) $ repeat 0)
  conflict_set <- conf set repPol
  let s1 = sum conflict_set
  let s2 = length conflict_set
  r <- evicts (CacheState (s1, s2)) repPol
  return r
   
conf :: [Int] -> RepPol -> IO([Int])
conf set pol = do
  r <- conf' set [] pol
  return r

conf' :: [Int] -> [Int] -> RepPol-> IO([Int])
conf' [] s _ = do return s
conf' set conf_set pol = do
  let len = length conf_set
  r <- probe (head set) conf_set pol
  if r
    then do rr <- conf' (tail set) conf_set pol
            return rr
    else do rr <- conf' (tail set) ((head set) : conf_set) pol
            return rr

probe :: Int -> [Int] -> RepPol -> IO(Bool)
probe 0 _ _ = do return False
probe _ set pol = do
  r <- evicts (CacheState(sum set, length set)) pol
  return r
  
-- Is True when reduction is succesful given a victim set and number of sets -- For probabilistic replacement policies
reduction :: CacheState -> RepPol -> IO(Bool)
reduction state@(CacheState(ev, total)) policy = do
  e2 <- evicts state policy
  let b = (associativity == total) && (e2)
  if b then return True
    else do
      c <- reduction_combinations state
      e <- findM (\x -> evicts x policy) c
      case e of
        Just new_state -> do
          r <- reduction new_state policy
          return r
        Nothing -> do return False

-- Is True when reduction is successful given a victim set and number of sets
reduction_noisy :: CacheState -> CacheState -> RepPol -> IO(Bool)
reduction_noisy state tlb@(CacheState(0, _)) policy = do
  r <- reduction state policy
  return r
reduction_noisy state@(CacheState(ev, total)) tlb@(CacheState(con, tlb_tot)) policy = do
  e2 <- evicts (CacheState(ev + con, total + tlb_tot)) policy
  let b = (associativity == total) && (e2)
  if b then return True
    else do
    c <- reduction_combinations state
    CacheState(cong, tlb_t) <- new_tlb_list $ expected_tlb_misses total
    let new_tlb = CacheState(cong, tlb_t)
    e <- findM (\(CacheState(ev, tot)) -> (evicts (CacheState(ev + cong, tot + tlb_t)) policy)) c
    case e of
     Just new_set -> do
       r <- reduction_noisy new_set new_tlb policy
       return r
     Nothing -> do return False
     
reduction_combinations :: CacheState -> IO([CacheState])
reduction_combinations state@(CacheState (ev, total)) = do
  r <- distribute_ev ev (map (\x -> CacheState (0, x)) aux) [0..associativity]
  -- let m = map (\(CacheState(x, t)) -> CacheState(ev - x, t)) r
  let m = zipWith (\(CacheState(x, t)) l -> CacheState (ev - x, total - l)) r aux
  return m
  where
    cei = ceiling $ (fromIntegral total) / (fromIntegral $ associativity + 1)
    trun = truncate $ (fromIntegral total) / (fromIntegral $ associativity + 1)
    n_cei = mod total (associativity + 1)
    n_trun = (associativity + 1) - n_cei
    aux = (take n_cei $ repeat cei) ++ (take n_trun $ repeat trun)
    
distribute_ev :: Int -> [CacheState] -> [Int] -> IO([CacheState])
distribute_ev 0 tups _ = do return tups
distribute_ev ev tups possib = do
  s <- sample $ randomElement possib
  let t = change s tups
  case t of
    Just tups' -> do
      r <- distribute_ev (ev - 1) tups' possib
      return r
    Nothing -> do
      let possib' = possib \\ [s]
      r <- distribute_ev ev tups possib'
      return r

change :: Int -> [CacheState] -> Maybe ([CacheState])
change s tups =
  case (tups!!s) of
    CacheState(_, 0) -> Nothing
    CacheState(ev, total) ->
      Just ((take s tups) ++ [CacheState(ev + 1, total - 1)] ++ (drop (s + 1) tups))

---- TLB
-- TLB misses for a set of addresses
tlb_misses :: [Int] -> Int
tlb_misses = sum . map (\x -> x - tlb_block_size) . filter (> tlb_block_size) . tlb_misses'
  
tlb_misses' :: [Int] -> [Int]
tlb_misses' tlbs = map (\x -> length $ filter (== x) tlbs) [0..((2^tlb_bits) - 1)]

-- Expected number of misses for a number of addresses
expected_tlb_misses :: Int -> Int
expected_tlb_misses n =
  let d = n - tlb_size
  in if d > 0 then d else 0
  
tlb_block_size :: Int
tlb_block_size = truncate $ (fromIntegral tlb_size) / (fromIntegral $ 2^tlb_bits)


---------------------- Aux

-- -- Delete nth element of a list
-- deleteN :: Int -> [a] -> [a]
-- deleteN _ []     = []
-- deleteN i (a:as)
--    | i == 0    = as
--    | otherwise = a : deleteN (i-1) as

-- Create n_bins combinations of 0 and 1
bins :: Int -> [[Int]]
bins n_bins = take (2^n_bins) $ filter((== n_bins) . length) $ concat $ iterate ((:) <$> [0, 1] <*>) [[]]


--  Like 'find', but where the test can be monadic.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p [] = return Nothing
findM p (x:xs) = ifM (p x) (return $ Just x) (findM p xs)

-- Like @if@, but where the test can be monadic.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f
