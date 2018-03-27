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

-- associativity (number of blocks per cache set)
associativity :: Int 
associativity = 12


------ TLB

-- Has to be multiple of tlb associativity
tlb_size :: Int
tlb_size = 1536

-- TLB associativity
tlb_bits :: Int
tlb_bits = 4

--------------------------------------------------------------------------------------------------------------
data Address = Address [Int]
  deriving (Read, Show, Eq)

showAddress :: Address -> [Int]
showAddress (Address a) = a

-- List of addresses as strings in lines
to_string_addresses :: [Address] -> String
to_string_addresses = unlines . map show . map showAddress
  
-- Only to be used with valid [Int] 
create_address :: [Int] -> Address
create_address = Address

 -- With a seed
createRandom_Address :: Int -> Int -> Address
createRandom_Address seed numberBits = Address (take numberBits $ randomRs (0, 1) (mkStdGen seed))


----- Address translation

-- From a virtual address, create a physical address with same offset and the rest random bits
virtual_to_physical_translation :: (Address, Int) -> Address
virtual_to_physical_translation (virtual, seed) = Address ((take (numberOfRandomBits) $ randomRs (0, 1) (mkStdGen seed)) ++ (drop (oldPageBits) $ showAddress virtual))
  where numberOfRandomBits = physical_address_length - pageOffset
        oldPageBits = virtual_address_length - pageOffset


---- Binary test

-- Returns 1 if victim address is in eviction set, otherwise 0
is_address_in_eviction_set :: Address -> [Address] -> Int
is_address_in_eviction_set victim set =
  if (length $ filter (== victims_set) $ get_sets_from_addresses set) >= associativity
  then 1
  else 0
  where victims_set = get_set_from_single_address victim
        
---- Polynomial test

exists_eviction :: [Address] -> Int
exists_eviction add =
  if (number_of_eviction_sets add) > 0
  then 1
  else 0

-- Returns number of caches with associativity number of addresses
number_of_eviction_sets :: [Address] -> Int
number_of_eviction_sets = length . filter (> associativity) . separate_addresses_into_bins


-- Takes list of addresses and outputs the histogram of sets(as a list) -> [3, 4, 4, 1] means there were 3 "00", 4 "01", 4 "10", 4 "11"
separate_addresses_into_bins :: [Address] -> [Int]
separate_addresses_into_bins sets = map (\x -> (length $ filter (==x) $ get_sets_from_addresses sets)) $ bins cacheSet
-- separate_addresses_into_bins :: [Address] -> [Int]
-- separate_addresses_into_bins sets = map length $ group $ sort $ get_sets_from_addresses sets


-- Takes list of addresses and outputs lists of the set part of the addresses (each part of the address is a list)
get_sets_from_addresses :: [Address] -> [[Int]]
get_sets_from_addresses = map (take cacheSet) . map (drop (physical_address_length - cacheOffset - cacheSet)) . map showAddress

-- Takes list of addresses and outputs lists of the set part of the addresses (each part of the address is a list)
get_set_from_single_address :: Address -> [Int]
get_set_from_single_address = take cacheSet . drop (physical_address_length - cacheOffset - cacheSet) . showAddress


reduction :: Address -> [Address] -> Int
reduction victim initial_set =
  if ((number_addreses == associativity) && (is_address_in_eviction_set victim initial_set) == 1)
  then 1
  else
    case (take_one victim combinations) of
      Just new_set -> reduction victim new_set
      Nothing -> 0 
  where
    number_addreses = length initial_set
    cei = ceiling $ (fromIntegral number_addreses) / (fromIntegral $ associativity + 1)
    trun = truncate $ (fromIntegral number_addreses) / (fromIntegral $ associativity + 1)
    n_cei = mod number_addreses (associativity + 1)
    n_trun = (associativity + 1) - n_cei
    aux = (take n_cei $ repeat cei) ++ (take n_trun $ repeat trun)
    groups = splitPlaces aux initial_set
    combinations = map (\x -> initial_set \\ x) groups


reduction_alt :: Int -> [Int] -> Int
reduction_alt v sets =
  if (number_addreses == associativity) && (is_eviction sets)
  then 1
  else
    case (find is_eviction combinations) of
      Just new_set -> reduction_alt v new_set
      Nothing -> 0
  where
    number_addreses = length sets
    cei = ceiling $ (fromIntegral number_addreses) / (fromIntegral $ associativity + 1)
    trun = truncate $ (fromIntegral number_addreses) / (fromIntegral $ associativity + 1)
    n_cei = mod number_addreses (associativity + 1)
    n_trun = (associativity + 1) - n_cei
    aux = (take n_cei $ repeat cei) ++ (take n_trun $ repeat trun)
    groups = splitPlaces aux sets
    combinations = map concat $ map (\x -> deleteN x groups) [0..((length groups) - 1)]
    is_eviction subs = if ((length $ filter (== v) subs) >= associativity) then True else False
        

deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as
   
-- delete_nth :: Int -> [a] -> [a]
-- delete_nth n [] = []
-- delete_nth n lst =
--   let (ys,zs) = splitAt n lst in
--     ys ++ (tail zs)


take_one :: Address -> [[Address]] -> Maybe [Address]
take_one vic = find (\x -> (is_address_in_eviction_set vic x) == 1)


-- Create set combinations
bins :: Int -> [[Int]]
bins n_bins = take (2^n_bins) $ filter((== n_bins) . length) $ concat $ iterate ((:) <$> [0, 1] <*>) [[]]


number_of_eviction_addresses :: [Address] -> Int
number_of_eviction_addresses = sum . filter (> associativity) . separate_addresses_into_bins

---- TLB
tlb_misses :: [Address] -> Int
tlb_misses = sum . map (\x -> x - tlb_block_size) . filter (> tlb_block_size) . tlb_misses' . map (take tlb_bits) . map (drop $ virtual_address_length - pageOffset - tlb_bits) . map showAddress
  
tlb_misses' :: [[Int]] -> [Int]
tlb_misses' pages = map (\x -> length $ filter (==x) pages) $ bins tlb_bits


tlb_block_size :: Int
tlb_block_size = truncate $ (fromIntegral tlb_size) / (fromIntegral $ 2^tlb_bits)
