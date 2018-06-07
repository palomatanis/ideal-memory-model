import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic

import Base
import Address_creation
import Cache_model
import Algorithms
     
instance Arbitrary Address where
  arbitrary = do
    a <- choose(0, free_cache - 1)
    return $ Address a

instance Arbitrary CacheState where
  arbitrary = do
    NonNegative total <- arbitrary
    congruent <- choose (0, total)
    return $ CacheState(congruent, total)
    
instance SetAddress where
  arbitrary = do
    n <- Arbitrary
    return $ SetAddress n
    
instance Arbitrary Trace where
  arbitrary = do
    l <- listOf SetAddress
    return $ Trace l

instance Arbitrary Set where
  arbitrary = do
    l <- listOf SetAddress
    return $ Set l
---- Propiedades de los generadores

--  List of addresses returns an address of the correct size for partial set list
prop_address_list_size_partial :: (NonNegative Int) -> Property
prop_address_list_size_partial (NonNegative n) = monadicIO $ do
  l <- run $ list_random_sets n random_set_partial
  let lenl = length l
  assert $ lenl == n
  
--  List of addresses returns an address of the correct size for complete set list
prop_address_list_size :: (NonNegative Int) -> Property
prop_address_list_size (NonNegative n) = monadicIO $ do
  l <- run $ list_random_sets n random_set
  let lenl = length l
  assert $ lenl == n

--  List of addresses returns list of addresses within correct range (0-free sets)
prop_address_list_range_partial :: (Positive Int) -> Property
prop_address_list_range_partial (Positive n) = monadicIO $ do
  l <- run $ list_random_sets n random_set_partial
  let max = maximum $ map (\(Address i) -> i) l
  let min = minimum $ map (\(Address i) -> i) l
  assert $ (max < free_cache) && (min >= 0)
  
--  List of addresses returns list of addresses within correct range (0-total sets)
prop_address_list_range :: (Positive Int) -> Property
prop_address_list_range (Positive n) = monadicIO $ do
  l <- run $ list_random_sets n random_set
  let max = maximum $ map (\(Address i) -> i) l
  let min = minimum $ map (\(Address i) -> i) l
  assert $ max < (2^cacheSet) && (min >= 0)

---- Propiedades tlb
prop_tlb_misses :: (NonNegative Int) -> Property
prop_tlb_misses (NonNegative n) = monadicIO $ do
  r <- run $ list_random_tlb n
  let t = tlb_misses r
  assert $ (t <= n) && (t >= 0)
  

-- Cache state created is of right size
prop_make_cache_state_size :: (NonNegative Int) -> Property
prop_make_cache_state_size (NonNegative n) = monadicIO $ do
  CacheState(c, t) <- run $ random_cacheState n
  assert $ (t == n) && (c <= t) && (c >= 0)

prop_evicts :: CacheState -> Property
prop_evicts cacheState@(CacheState(congr, total)) = monadicIO $ do
  e <- run $ evicts cacheState lru
  if (congr >= associativity)
    then assert e
    else assert $ not e  

prop_rep_empty_trace :: Set -> Property
prop_rep_empty_trace set = monadicIO $ do
  (s, h) <- cacheInsert lru set (Trace 0)
  assert $ s == set

-- Group reduction succeeds when there are associativity or more congruent addresses
prop_group_reduction_bool :: CacheState -> Property
prop_group_reduction_bool cacheState@(CacheState(congr, total)) = monadicIO $ do
  r <- run $ reduction cacheState lru
  if (congr >= associativity)
    then assert r
    else assert $ not r

-- Group reduction succeeds when there are associativity or more congruent addresses
prop_linear_reduction_bool :: CacheState -> Property
prop_linear_reduction_bool cacheState@(CacheState(congr, total)) = monadicIO $ do
  r <- run $ naive_reduction cacheState lru
  if (congr >= associativity)
    then assert r
    else assert $ not r

-- Checks that group reduction returns assoc addresses
prop_group_reduction :: CacheState -> Property
prop_group_reduction cacheState@(CacheState(congr, total)) = monadicIO $ do
  CacheState(c, t) <- run $ reduction_b cacheState lru
  if (congr >= associativity)
    then assert $ (c == t) && (t == associativity)
    else assert $ (c == congr) && (t == total)

-- Checks that linear reduction returns assoc addresses
prop_linear_reduction :: CacheState -> Property
prop_linear_reduction cacheState@(CacheState(congr, total)) = monadicIO $ do
  CacheState(c, t) <- run $ naive_reduction_b cacheState lru
  if (congr >= associativity)
    then assert $ (c == t) && (t == associativity)
    else assert $ (c == congr) && (t == total)
    
    
main :: IO ()
main = do
  putStrLn "Created list of addresses is correct size 1"
  quickCheck prop_address_list_size_partial
  
  putStrLn "Created list of addresses is correct size 2"
  quickCheck prop_address_list_size

  putStrLn "Created list of addresses are in correct range 1"
  quickCheck prop_address_list_range_partial
  
  putStrLn "Created list of addresses are in correct range 2"
  quickCheck prop_address_list_range

  putStrLn "Created cache state is of correct size"
  quickCheck prop_make_cache_state_size

  putStrLn "TLB misses are within range"
  quickCheck prop_tlb_misses
  
  putStrLn "When there are more than associativity addresses, the set is evicting"
  quickCheck prop_evicts
  
  putStrLn "Group reduction succeeds when there are at least associativity addresses"
  quickCheck prop_group_reduction_bool

  putStrLn "Linear reduction succeeds when there are at least associativity addresses"
  quickCheck prop_linear_reduction_bool

  putStrLn "Group reduction returns a CacheState of the form (CacheState(assoc, assoc))"
  quickCheck prop_group_reduction  

  putStrLn "Linear reduction returns a CacheState of the form (CacheState(assoc, assoc))"
  quickCheck prop_linear_reduction  
