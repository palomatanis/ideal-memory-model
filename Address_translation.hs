module Address_translation where

import System.Random
import Data.List

----- Memory parameters

-- Number of bits of the virtual address
virtualAddressLength :: Int
virtualAddressLength = 32

-- Number of bits of the physical address
physicalAddressLength :: Int
physicalAddressLength = 30

-- Bits of offset, between 0 and addressLength
pageOffset :: Int
pageOffset = 10

----- Cache parameters

-- Total cache size?

-- Number of offset bits (n bits for blocks of 2^n)
offsetCacheBits :: Int
offsetCacheBits = 7

-- Number of bits for set (s bits for 2^s sets)
setBits :: Int
setBits = 5

-- Associativity (number of blocks per cache)
associativity = 4


data Address = Address [Int]

---- Creating addresses

showAddress :: Address -> [Int]
showAddress (Address a) = a

-- Only to be used with valid addresses 
createAddress :: [Int] -> Address
createAddress address = Address address

-- With a seed
createRandom_Address :: (Int, Int) -> Address
createRandom_Address (seed, numberBits) = Address (take numberBits $ randomRs (0, 1) (mkStdGen seed))


-- From a virtual address, create a physical address with same offset and the rest random bits
createRandom_PhysicalFromVirtual :: (Address, Int) -> Address
createRandom_PhysicalFromVirtual (virtual, seed) = Address ((take (numberOfRandomBits) $ randomRs (0, 1) (mkStdGen seed)) ++ (drop (oldPageBits) $ showAddress virtual))
  where numberOfRandomBits = physicalAddressLength - pageOffset
        oldPageBits = virtualAddressLength - pageOffset

-- Takes list of addresses and outputs lists of the set part of the addresses (each part of the address is a list)
getSetsFromAddresses :: [Address] -> [[Int]]
getSetsFromAddresses addrs = map (take setBits) $ map (drop (physicalAddressLength - offsetCacheBits - setBits)) $ map showAddress addrs

-- Takes list of addresses and outputs the histogram of sets(as a list) -> [3, 4, 4, 1] means there were 3 "00", 4 "01", 4 "10", 4 "11"
separateAdressesIntoBins :: [Address] -> [Int]
separateAdressesIntoBins sets = map length $ group $ sort $ getSetsFromAddresses sets

-- Returns number of caches with associativity number of addresses
checkEvictionSet :: [Int] -> Int
checkEvictionSet bins = length $ filter (> associativity) bins
