module Address_translation  where

import System.Random
import Data.List
import Data.List.Split

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
associativity = 12


------ TLB

-- Has to be multiple of tlb associativity
tlb_size :: Int
tlb_size = 1536

-- TLB associativity
tlb_bits :: Int
tlb_bits = 2

--------------------------------------------------------------------------------------------------------------

data VAddress = VAddress [Int]
  deriving (Read, Show, Eq)

data PAddress = PAddress [Int]
  deriving (Read, Show, Eq)

data Set = Set Int
  deriving (Read, Show, Eq)


showVAddress :: VAddress -> [Int]
showVAddress (VAddress a) = a

showPAddress :: PAddress -> [Int]
showPAddress (PAddress a) = a
  
show_set :: Set -> Int
show_set (Set a) = a


-- Only to be used with valid [Int] 
create_v_address :: [Int] -> VAddress
create_v_address = VAddress

-- Only to be used with valid [Int] 
create_p_address :: [Int] -> PAddress
create_p_address = PAddress

-- Only to be used with valid [Int] 
create_set :: Int -> Set
create_set = Set

 -- With a seed
createRandom_VAddress :: Int -> Int -> VAddress
createRandom_VAddress seed numberBits = VAddress (take numberBits $ randomRs (0, 1) (mkStdGen seed))

-- With a seed
createRandom_PAddress :: Int -> Int -> PAddress
createRandom_PAddress seed numberBits = PAddress (take numberBits $ randomRs (0, 1) (mkStdGen seed))

----- Address translation

-- From a virtual address, create a physical address with same offset and the rest random bits
virtual_to_physical_translation :: (VAddress, Int) -> PAddress
virtual_to_physical_translation (virtual, seed) = PAddress ((take (numberOfRandomBits) $ randomRs (0, 1) (mkStdGen seed)) ++ (drop (oldPageBits) $ showVAddress virtual))
  where numberOfRandomBits = physical_address_length - pageOffset
        oldPageBits = virtual_address_length - pageOffset


---- Binary test

-- Returns True if victim address is in eviction set
evicts :: [Set] -> Set -> Bool
evicts set victim = (length $ filter (== victim) set) >= associativity

                                        
-- Is there at least one eviction set
exists_eviction :: [Set] -> Bool
exists_eviction add = (number_of_eviction_sets add) > 0


-- Count number of addresses in eviction sets
number_of_eviction_addresses :: [Set] -> Int
number_of_eviction_addresses = sum . filter (> associativity) . separate_sets_into_bins


-- Returns number of eviction sets for a list of sets
number_of_eviction_sets :: [Set] -> Int
number_of_eviction_sets = length . filter (> associativity) . separate_sets_into_bins


-- Takes list of sets and outputs the histogram
separate_sets_into_bins :: [Set] -> [Int]
separate_sets_into_bins sets = map (\x -> (length $ filter (==x) $ map show_set sets)) $ [0..(free_cache - 1)]


-- Is True when reduction is succesful given a victim set and number of sets
reduction :: Set -> [Set] -> Bool
reduction v sets =
  ((number_addresses == associativity) && (evicts sets v))
  ||
  (case (find (\s -> evicts s v) $ reduction_combinations sets) of
     Just new_set -> reduction v new_set
     Nothing -> False)
  where
    number_addresses = length sets


-- Is True when reduction is successful given a victim set and number of sets
reduction_noisy :: Set -> [Set] -> [Set] -> Bool
reduction_noisy v sets [] = reduction v sets
reduction_noisy v sets tlb_list =
  ((number_addresses == associativity) && (evicts (sets ++ tlb_list) v))
  ||
  (case (find (\x -> evicts (x ++ (new_tlb_list x)) v) $ reduction_combinations sets) of
     Just new_set -> reduction_noisy v new_set $ new_tlb_list new_set
     Nothing -> False)
  where
    new_tlb_list s = take (expected_tlb_misses $ length s) tlb_list
    number_addresses = length sets

    
reduction_combinations :: [Set] -> [[Set]]
reduction_combinations sets = map concat $ map (\x -> deleteN x groups) [0..((length groups) - 1)]
  where
    number_addresses = length sets
    cei = ceiling $ (fromIntegral number_addresses) / (fromIntegral $ associativity + 1)
    trun = truncate $ (fromIntegral number_addresses) / (fromIntegral $ associativity + 1)
    n_cei = mod number_addresses (associativity + 1)
    n_trun = (associativity + 1) - n_cei
    aux = (take n_cei $ repeat cei) ++ (take n_trun $ repeat trun)
    groups = splitPlaces aux sets

  
---- TLB
tlb_misses :: [Int] -> Int
tlb_misses = sum . map (\x -> x - tlb_block_size) . filter (> tlb_block_size) . tlb_misses'
  
tlb_misses' :: [Int] -> [Int]
tlb_misses' tlbs = map (\x -> length $ filter (== x) tlbs) [0..((2^tlb_bits) - 1)]

-- Expected number of misses
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
