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

data Address = Address [Int]

showAddress :: Address -> [Int]
showAddress (Address a) = a


-- Seed?

seedNumber :: Int
seedNumber = 123222
  
createRandom_PhysicalAddress :: Address
createRandom_PhysicalAddress = Address (take physicalAddressLength $ randomRs (0, 1) (mkStdGen seedNumber))


createRandom_VirtualAddress :: Address
createRandom_VirtualAddress = Address (take virtualAddressLength $ randomRs (0, 1) (mkStdGen seedNumber))

-- From a virtual address, create a physical addres with same offset and the rest random bits
createRandom_PhysicalFromVirtual :: Address -> Address
createRandom_PhysicalFromVirtual virtual = Address ((take (numberOfRandomBits) $ randomRs (0, 1) (mkStdGen seedNumber)) ++ (drop (oldPageBits) $ showAddress virtual))
  where numberOfRandomBits = physicalAddressLength - pageOffset
        oldPageBits = virtualAddressLength - pageOffset



-- createRandomPhysicalAddress_seed :: (Address, StdGen) 
-- createRandomPhysicalAddress_seed = (Address addr, newGen)
--   where (addr, newGen) = take physicalAddressLength $ randomR (0, 1) (mkStdGen seed)


createRandom_PhysicalAddressList :: Int -> [Address]
createRandom_PhysicalAddressList 0 = [] 
createRandom_PhysicalAddressList n = createRandom_PhysicalAddress : (createRandom_PhysicalAddressList (n-1))


-- Takes list of addresses and outputs lists of the set part of the addresses (each part of the address is a list
getSetsFromAddresses :: [Address] -> [[Int]]
getSetsFromAddresses addrs = map (take setBits) $ map (drop (physicalAddressLength - offsetCacheBits - setBits)) $ map showAddress addrs

-- Takes list of addresses and outputs the histogram of sets  ->  [3, 4, 4, 1] means there were 3 "00", 4 "01", 4 "10", 4 "11"
separateAdressesIntoBins :: [Address] -> [Int]
separateAdressesIntoBins sets = map length $ group $ sort $ getSetsFromAddresses sets
