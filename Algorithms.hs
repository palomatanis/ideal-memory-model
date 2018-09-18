module Algorithms  where

import System.Random.Shuffle
import Data.List
import Data.Random

import Base
import Address_creation
import Cache_model

import Control.Monad
  
evicts :: SetState -> RepPol -> IO(Bool)
evicts (SetState (c, n)) policy = do
   let (Trace t) = consecutive_trace c
   (s, h) <- cacheInsert policy initialSet (Trace ((AddressIdentifier (c + 1)) : t)) n
   (s2, Hit h2) <- cacheInsert policy s (Trace [(AddressIdentifier(c + 1))]) 1
   let v = h2 == 0
   return v


eviction_strategy :: EvictionStrategy
eviction_strategy = (1, 1, 1)


-- Eviction test for adaptive replacement policy
evicts_adapt :: SetAddresses -> RepPol -> RepPol -> IO(Bool)
evicts_adapt set@(SetAddresses s) rep1 rep2 = do
  -- change initial set by set with the victim already in
  let n = length s
  (t, _, _) <- adaptiveCacheInsert rep1 rep2 initialSet (SetAddresses [LongAddress((AddressIdentifier n), (Address 2))])
  (t2, _, _) <- adaptiveCacheInsert rep1 rep2 t (eviction_strategy_trace set eviction_strategy)
  (_, Hit h3, _) <- adaptiveCacheInsert rep1 rep2 t2 (SetAddresses [LongAddress((AddressIdentifier n), (Address 2))])
  let v = h3 == 0
  return v

-- Creates the set of addresses to test eviction with a certain eviction strategy
eviction_strategy_trace :: SetAddresses -> EvictionStrategy -> SetAddresses
eviction_strategy_trace (SetAddresses set) strategy = (SetAddresses (map (\x -> set !! x) $ eviction_strategy_trace' set strategy 0))

eviction_strategy_trace' :: [LongAddress] -> EvictionStrategy -> Int -> [Int]
eviction_strategy_trace' [] (c, d, l) n = []
eviction_strategy_trace' set (c, d, l) n = (take (c * d) $ cycle [n..(n+d-1)]) ++ (if (n > ((length set) - l - 1)) then [] else eviction_strategy_trace' set (c, d, l) (n + l))


-- Evictschance returns true if the set evicts at least 80%
chanceMin :: Int
chanceMin = 1
chanceOutOf :: Int
chanceOutOf = 1

evictschance :: SetState -> RepPol -> IO(Bool)
evictschance set pol = do
  r <- replicateM chanceOutOf $ evicts set pol
  let s = (length $ filter (== True) r)
  let b = s >= chanceMin
  return b

  
type ReductionAlgorithm = SetState -> RepPol -> IO(Maybe(SetState))

reduction_size :: Int
reduction_size = associativity

-- Is True when reduction is succesful given a cacheState and a replacement policy
reduction :: ReductionAlgorithm
reduction state@(SetState(c, n)) policy = do
  let b = (c == reduction_size) && (c == n)
  if b then return (Just(state))
    else do
      b <- reduction_combinations state
      e <- findM (\x -> evictschance x policy) b
      case e of
        Just new_state -> do
          r <- reduction new_state policy
          return r
        Nothing -> do return Nothing
        
-- Baseline reduction algorithm
baseline_reduction :: ReductionAlgorithm
baseline_reduction state repPol = do
  e <- evictschance state repPol
  if (e)
    then do
    r <- baseline_reduction' state repPol (SetState(0, 0))
    return r
    else return Nothing

-- The Ints counts how many congruent addresses have been taken already, and how many in total
baseline_reduction' :: SetState -> RepPol -> SetState -> IO(Maybe(SetState))    
baseline_reduction' state@(SetState(c, t)) repPol eviction_set@(SetState(ec, et))= do
  if (ec == reduction_size && et == reduction_size)
    then do return (Just(eviction_set))
    else do if (c == 0 || et > ec)
              then do return Nothing
              else do (newState@(SetState(fc, ft)), taken) <- take1 state
                      x <- evictschance (SetState(fc + ec, ft + et)) repPol
                      if (x)
                        then do o <- baseline_reduction' newState repPol eviction_set
                                return o
                        else do o <- baseline_reduction' newState repPol (SetState(ec + taken, et + 1))
                                return o

-- Naive reduction for replacement policies
naive_reduction :: ReductionAlgorithm
naive_reduction state@(SetState(c, n)) repPol = do
  set <- shuffleM $ (take c $ repeat 1) ++ (take (n - c) $ repeat 0)
  conflict_set <- conf set repPol
  eviction_set <- ev_set conflict_set repPol
  let s1 = sum eviction_set
  let s2 = length eviction_set
  r <- evictschance (SetState (s1, s2)) repPol
  if (r)
    then return (Just (state))
    else return (Nothing)
    

probe :: Int -> [Int] -> RepPol -> IO(Bool)
probe 0 _ _ = do return False
probe _ set pol = do
  r <- evictschance (SetState(sum set, length set)) pol
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
reduction_combinations :: SetState -> IO([SetState])
reduction_combinations state@(SetState (ev, n)) = do
  r <- distribute_ev ev (map (\x -> SetState (0, x)) aux) [0..reduction_size]
  -- let m = map (\(SetState(x, t)) -> SetState(ev - x, t)) r
  let m = zipWith (\(SetState(x, t)) l -> SetState (ev - x, n - l)) r aux
  return m
  where
    cei = ceiling $ (fromIntegral n) / (fromIntegral $ reduction_size + 1)
    trun = truncate $ (fromIntegral n) / (fromIntegral $ reduction_size + 1)
    n_cei = mod n (reduction_size + 1)
    n_trun = (reduction_size + 1) - n_cei
    aux = (take n_cei $ repeat cei) ++ (take n_trun $ repeat trun)

-- Aux for reduction_combination
distribute_ev :: Int -> [SetState] -> [Int] -> IO([SetState])
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


change :: Int -> [SetState] -> Maybe ([SetState])
change s tups =
  case (tups!!s) of
    SetState(_, 0) -> Nothing
    SetState(ev, n) ->
      Just ((take s tups) ++ [SetState(ev + 1, n - 1)] ++ (drop (s + 1) tups))


take1 :: SetState -> IO(SetState, Int)
take1 state@(SetState(e, n)) = do
  f <- chanceX e n
  return ((SetState(e-f, n-1)), f)
    
