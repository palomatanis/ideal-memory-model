module Base  where
  
----- TYPES

-- Address represented as an Int (set number)
newtype Address = Address Int
  deriving (Read, Show, Eq)

-- The cache state is represented by the number of addresses in the victim set, and the total number of addresses
newtype SetState = SetState(Int, Int)
  deriving (Read, Show, Eq)

-- Congruent addresses, represented with numbers to distinguist each different address
newtype AddressIdentifier = AddressIdentifier Int
  deriving (Read, Show, Eq)

-- List of congruent addresses that are going to be inserted to a cache
newtype Trace = Trace [AddressIdentifier]
  deriving (Read, Show, Eq)

-- Content of a cache set, represented as the list of addresses inside it
newtype CacheSetContent = CacheSetContent [AddressIdentifier]
  deriving (Read, Show, Eq)

-- Number of hits after a trace is put inside a cache
newtype HitNumber = Hit Int
  deriving (Read, Show, Eq)

-- -- Set of addresses, each address is a tuple with a unique ID and the set number
data LongAddress = LongAddress (AddressIdentifier, Address)
  deriving (Read, Show, Eq)

data SetAddresses = SetAddresses [LongAddress]
  deriving (Read, Show, Eq)

type RepPol = CacheSetContent -> Trace -> IO(CacheSetContent, HitNumber)

type CacheState = (RepPol, RepPol, CacheSetContent, [(Int, CacheSetContent)], HitNumber, Int)

type EvictionStrategy = (Int, Int, Int)

create_set :: Int -> Address
create_set = Address

show_set :: Address -> Int
show_set (Address a) = a


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

-- Slicing
slice_bits :: Int
slice_bits = 0

-- Actual number of cache bits that can change
free_cache_bits :: Int
--free_cache_bits = cacheOffset + cacheSet - pageOffset
free_cache_bits = cacheOffset + cacheSet - pageOffset + slice_bits

free_cache :: Int
free_cache = 2^free_cache_bits
  
-- associativity (number of blocks per cache set)
associativity :: Int 
associativity = 16

------ TLB
-- Has to be multiple of tlb associativity
tlb_size :: Int
tlb_size = 1536

-- TLB associativity (bits for tlb associativity) -> 2 means assoc 4
tlb_bits :: Int
tlb_bits = 2


--- Number of regions or constituencies for the adaptive rep. pol
num_regions :: Int
num_regions = 32

