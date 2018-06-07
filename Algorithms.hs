module Algorithms  where

import System.Random.Shuffle
import Data.List
import Data.Random

import Base
import Address_creation
import Cache_model

  
evicts :: CacheState -> RepPol -> IO(Bool)
evicts (CacheState (n, total)) policy = do
   let (Trace t) = consecutive_trace n
   (s, h) <- cacheInsert policy initialSet (Trace ((SetAddress (n + 1)) : t)) total
   (s2, Hit h2) <- cacheInsert policy s (Trace [(SetAddress(n + 1))]) 1
   let v = h2 == 0
   return v
  
type ReductionAlgorithm = CacheState -> RepPol -> IO(Bool)

-- Is True when reduction is succesful given a cacheState and a replacement policy
reduction :: ReductionAlgorithm
reduction state@(CacheState(ev, total)) policy = do
  e2 <- evicts state policy
  let b = (ev >= associativity) && (e2) && (ev == total)
  if b then return True
    else do
      c <- reduction_combinations state
      e <- findM (\x -> evicts x policy) c
      case e of
        Just new_state -> do
          r <- reduction new_state policy
          return r
        Nothing -> do return False 
        

-- Naive reduction for replacement policies
naive_reduction :: ReductionAlgorithm
naive_reduction state@(CacheState(ev, total)) repPol = do
  set <- shuffleM $ (take ev $ repeat 1) ++ (take (total - ev) $ repeat 0)
  conflict_set <- conf set repPol
  eviction_set <- ev_set conflict_set repPol
  let s1 = sum eviction_set
  let s2 = length eviction_set
  r <- evicts (CacheState (s1, s2)) repPol
  return r


reduction_b :: CacheState -> RepPol -> IO(CacheState)
reduction_b state@(CacheState(ev, total)) policy = do
  e2 <- evicts state policy
  let b = (ev <= associativity) && (e2) && (ev == total)
  if b then return state
    else do
      c <- reduction_combinations state
      e <- findM (\x -> evicts x policy) c
      case e of
        Just new_state -> do
          r <- reduction_b new_state policy
          return r
        Nothing -> do return state


-- Naive reduction for replacement policies
naive_reduction_b :: CacheState -> RepPol -> IO(CacheState)
naive_reduction_b state@(CacheState(ev, total)) repPol = do
  set <- shuffleM $ (take ev $ repeat 1) ++ (take (total - ev) $ repeat 0)
  conflict_set <- conf set repPol
  eviction_set <- ev_set conflict_set repPol
  let s1 = sum eviction_set
  let s2 = length eviction_set
  r <- evicts (CacheState (s1, s2)) repPol
  return (CacheState (s1, s2))

    

probe :: Int -> [Int] -> RepPol -> IO(Bool)
probe 0 _ _ = do return False
probe _ set pol = do
  r <- evicts (CacheState(sum set, length set)) pol
  return r
     
--- Aux
conf :: [Int] -> RepPol -> IO([Int])
conf set pol = do
  r <- conf' set [] pol
  return r
  where
    conf' [] s _ = do return s
    conf' set conf_set pol = do
      let h = head set
      let t = tail set 
      r <- probe h conf_set pol
      if r
        then do rr <- conf' t conf_set pol
                return rr
        else do rr <- conf' t (h : conf_set) pol
                return rr


ev_set :: [Int] -> RepPol -> IO([Int])
ev_set conf_set pol = do
  r <- ev_set' conf_set conf_set [] pol
  return r
  where
    ev_set' _ [] ev _ = do return ev
    ev_set' conf_set acc_set ev pol = do
      let h = head acc_set
      e <- probe 1 (conf_set \\ [h]) pol
      if e
        then do rr <- ev_set' conf_set (tail acc_set) ev pol
                return rr
        else do rr <- ev_set' conf_set (tail acc_set) (h : ev) pol
                return rr

-- Aux for reduction     
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

-- Aux for reduction_combination
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




