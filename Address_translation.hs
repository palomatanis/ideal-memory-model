module Address_translation  where

import System.Random
import Data.List
import Data.List.Split

import Data.Random

-------------- PARAMETERS
----- Address

virtual_address_length :: Int
virtual_address_length = 48

physical_address_length :: Int
physical_address_length = 34

-- Bits of page index , between 0 and addressLength
pageOffset :: Int
pageOffset = 12

----- Cache

-- Number of offset bits (n bits for blocks of 2^n)
cacheOffset :: Int
cacheOffset = 6

-- Number of bits for set (s bits for 2^s sets)
cacheSet :: Int
cacheSet = 13

-- Actual number of cache bits that can change
free_cache_bits :: Int
free_cache_bits = cacheOffset + cacheSet - pageOffset

free_cache :: Int
free_cache = 2^free_cache_bits
  
-- associativity (number of blocks per cache set)
associativity :: Int 
associativity = 16


------ TLB
-- Has to be multiple of tlb associativity
tlb_size :: Int
tlb_size = 1536

-- TLB associativity (bits for tlb associativity)
tlb_bits :: Int
tlb_bits = 2

--------------------------------------------------------------------------------------------------------------

data Address = Address Int
  deriving (Read, Show, Eq)

-- The cache state is represented by the number of addresses in the victim set, and the total number of addresses
data CacheState = CacheState(Int, Int)
  deriving (Read, Show, Eq)

show_set :: Address -> Int
show_set (Address a) = a

show_cach :: CacheState -> (Int, Int)
show_cach (CacheState (a, b)) = (a, b)

create_set :: Int -> Address
create_set = Address

---- Binary test

-- Returns True if victim address is in eviction set with a probability of failure
evicts :: CacheState -> IO(Bool)
evicts (CacheState (ev, n)) = chance $ prob ev n

-- Receives probability and throws a coin with that prob
chance :: Int -> IO(Bool)
chance nu = do
  let distr = (take nu $ repeat True) ++ (take (100 - nu) $ repeat False)
  r <- sample $ randomElement distr
  return r
  
-- Receives number of addresses in eviction set and returns probability of an eviction set
prob :: Int -> Int -> Int
prob ev n
  | ev < 16 = 0
  | ev < 20 = if (n < 1800) then 90 else 60
  | ev <= 24 = if (n < 2400) then 80 else 60
  | otherwise = 100

  
-- -- Receives number of addresses in eviction set and returns probability of an eviction set
-- prob :: Int -> Int -> Int
-- prob ev n
--   | ev < 16 = 0
--   | ev < 20 = if (n < 1800) then 100 else 75
--   | ev <= 24 = if (n < 2400) then 100 else 75
--   | otherwise = 100

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


-- Is True when reduction is succesful given a victim set and number of sets
reduction :: CacheState -> IO(Bool)
reduction state@(CacheState(ev, total)) = do
  e2 <- evicts state
  let b = (associativity == total) && (e2)
  if b then return True
    else do
      c <- reduction_combinations state
      e <- findM evicts c
      case e of
        Just new_state -> do
          r <- reduction new_state
          return r
        Nothing -> do return False
        
    
-- -- Is True when reduction is successful given a victim set and number of sets
-- reduction_noisy :: Address -> [Address] -> [Address] -> IO(Bool)
-- reduction_noisy v sets [] = do
--   r <- reduction v sets
--   return r
-- reduction_noisy v sets tlb_list = do
--   let number_addresses = length sets
--   e2 <- evicts2 (sets ++ tlb_list) v
--   let b = (number_addresses <= associativity) && (e2)
--   if b then return True
--     else do
--     e <- findM (\x -> evicts2 (x ++ (new_tlb_list x)) v) $ reduction_combinations sets
--     case e of
--      Just new_set -> do
--        r <- reduction_noisy v new_set $ new_tlb_list new_set
--        return r
--      Nothing -> do return False
--   where
--     new_tlb_list s = take (expected_tlb_misses $ length s) tlb_list
--     number_addresses = length sets

                       
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

-- Delete nth element of a list
deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as

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
