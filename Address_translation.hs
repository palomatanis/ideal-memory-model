module Address_translation where

import System.Random
import Data.List

----- Address parameters

virtualAddressLength :: Int
virtualAddressLength = 48

physicalAddressLength :: Int
physicalAddressLength = 34

-- Bits of offset, between 0 and addressLength
pageOffset :: Int
pageOffset = 12

----- Cache parameters

-- Number of offset bits (n bits for blocks of 2^n)
cacheOffset :: Int
cacheOffset = 6

-- Number of bits for set (s bits for 2^s sets)
cacheSet :: Int
cacheSet = 13

-- Associativity (number of blocks per cache set)
associativity :: Int 
associativity = 12


------ TLB

-- bits
tlb_size :: Int
tlb_size = 1536
-- tlb_size = 10


--------------------------------------------------------------------------------------------------------------
data Address = Address [Int]

showAddress :: Address -> [Int]
showAddress (Address a) = a

-- List of addresses as strings in lines
toStringAddresses :: [Address] -> String
toStringAddresses = unlines . map show . map showAddress
  
-- Only to be used with valid [Int] 
createAddress :: [Int] -> Address
createAddress = Address

 -- With a seed
createRandom_Address :: Int -> Int -> Address
createRandom_Address seed numberBits = Address (take numberBits $ randomRs (0, 1) (mkStdGen seed))


-----

-- From a virtual address, create a physical address with same offset and the rest random bits
virtual_to_physical_translation :: (Address, Int) -> Address
virtual_to_physical_translation (virtual, seed) = Address ((take (numberOfRandomBits) $ randomRs (0, 1) (mkStdGen seed)) ++ (drop (oldPageBits) $ showAddress virtual))
  where numberOfRandomBits = physicalAddressLength - pageOffset
        oldPageBits = virtualAddressLength - pageOffset
        

-- Returns number of caches with associativity number of addresses
number_of_eviction_sets :: [Address] -> Int
number_of_eviction_sets = length . filter (> associativity) . separateAdressesIntoBins


-- Takes list of addresses and outputs the histogram of sets(as a list) -> [3, 4, 4, 1] means there were 3 "00", 4 "01", 4 "10", 4 "11"
separateAdressesIntoBins :: [Address] -> [Int]
separateAdressesIntoBins sets = map (\x -> (length $ filter (==x) $ getSetsFromAddresses sets)) bins

-- separateAdressesIntoBins :: [Address] -> [Int]
-- separateAdressesIntoBins sets = map length $ group $ sort $ getSetsFromAddresses sets


-- Takes list of addresses and outputs lists of the set part of the addresses (each part of the address is a list)
getSetsFromAddresses :: [Address] -> [[Int]]
getSetsFromAddresses = map (take cacheSet) . map (drop (physicalAddressLength - cacheOffset - cacheSet)) . map showAddress


-- Create set combinations
bins :: [[Int]]
bins = take (2^cacheSet) $ filter((== cacheSet) . length) $ concat $ iterate ((:) <$> [0, 1] <*>) [[]]


number_of_eviction_addresses :: [Int] -> Int
number_of_eviction_addresses = sum . filter (> associativity)
