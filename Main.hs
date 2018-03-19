module Main where

import System.Random
import Address_translation
import Control.Monad
import System.IO
import Data.List

-- media de 1000 repeticiones
-- entre 0 y 4000 direcciones. intervalo asociatividad

numberAddrToTest_From :: Int
numberAddrToTest_From = 0

numberAddrToTest_To :: Int
numberAddrToTest_To = 4000
-- numberAddrToTest_To = 4000

iterations :: Int
iterations = 100
-- iterations = 1000

memoryRange :: Int
memoryRange = 24


printing = do
  p <- list_semi_random_addresses 100 memoryRange
  putStrLn $ toStringAddresses p
  
-- Save tests
main = do
    file <- openFile "resultsBlaBla.txt" WriteMode
    m <- test_complete
    hPrint file $ list_to_string m
    hClose file
    where list_to_string = unwords . map show
    
test_complete = do
  p <- mapM test_n_addresses [numberAddrToTest_From, (numberAddrToTest_From + (2*associativity))..numberAddrToTest_To]
  return p
  
-- Mean of eviction sets for n addresses
test_n_addresses :: Int -> IO(Float)  
test_n_addresses n = do
  p <-  replicateM iterations $ count_evictions n
  return (mean p)

         
-- Creates list of n physical addresses from a virtual address, and counts eviction sets
-- The list is created by choosing n addresses with the same offset and belonging to the same range but each of a different page 
count_evictions :: Int -> IO(Int)
count_evictions number = do
  listR <- generateSeeds number
  r <- list_semi_random_addresses number memoryRange
  return (number_of_eviction_sets $ map virtual_to_physical_translation $ zip r listR)

-- create list of n [0...0] addresses of length nbits (virtual)
list_of_addresses_same_offset :: Int -> Int -> [Address]
list_of_addresses_same_offset _ 0 = []
list_of_addresses_same_offset nbits n = (createAddress $ take nbits [0,0..0]):list_of_addresses_same_offset nbits (n-1)


-- Simplificacion??
-- Generates list of n addesses with the same offset [0...0] in a range of 2^range addresses, each from a different page
-- Range a in bits
list_semi_random_addresses :: Int -> Int -> IO ([Address])
list_semi_random_addresses n range = do
  s <- randomIO :: IO(Int)
  let start = take (virtualAddressLength - pageOffset) $ randomRs (0, 1) (mkStdGen s)
  let fixed_bits = take number_fixed_bits start
  p <- n_different_pages number_variable_bits n
  return (map createAddress $ map (\x -> fixed_bits ++ x ++ offset) p)
  where
    number_fixed_bits = virtualAddressLength - range
    number_variable_bits = range - pageOffset
    offset = take pageOffset [0,0..0]

-- Takes a number of bits, and the length  and returns a list of random, not repeated [Int]
n_different_pages :: Int -> Int -> IO ([[Int]])
n_different_pages n_bits len = n_different_pages' n_bits len []
  -- if ((2^n_bits) < len)
  --   then do
  --      p <- list_of_addresses_same_offset n_bits len
  --      return (map showAddress p)
  --   else do
  --      (return (n_different_pages' n_bits len []))
  
n_different_pages' :: Int -> Int -> [[Int]] -> IO ([[Int]])
n_different_pages' n_bits len original_list = do
  let actual_list = nub original_list
  let length_difference = len - (length actual_list)
  if (length_difference == 0)
    then return actual_list
    else do
       l <- list_of_random_addresses n_bits length_difference
       let new_list = nub $ map showAddress l
       r <- n_different_pages' n_bits len (actual_list ++ new_list)
       return r

  
  

--   -- Takes a number of bits, and the length  and returns a list of random, not repeated [Int]
-- n_different_pages :: Int -> Int -> IO ([[Int]])
-- n_different_pages n_bits len = do
--   l <- list_of_random_addresses n_bits len
--   let actual_list = nub $ map showAddress l
--   let length_l = length actual_list
--   if (length_l == len)
--     then return (map showAddress l)
--     else do
--        r <- n_different_pages n_bits (len - length_l)
--        return (actual_list ++ r)


-- create list of n random addresses of length len (virtual)
list_of_random_addresses :: Int -> Int-> IO ([Address])
list_of_random_addresses _ 0 = return []
list_of_random_addresses nbits n = do
  r <- random_address nbits
  rs <- list_of_random_addresses nbits (n-1)
  return (r:rs)
  

-- Create random address of length n
random_address :: Int -> IO (Address)
random_address nbits = do
  newRand <- randomIO
  return (createRandom_Address newRand nbits)


-- Generates a list of n seeds
generateSeeds :: Int -> IO ([Int])
generateSeeds 0 = return []
generateSeeds n = do
  r <- randomIO
  rs <- generateSeeds (n-1)
  return (r:rs)


-- Mean of a list
mean :: [Int] -> Float
mean l = (fromIntegral $ sum l)/(fromIntegral $ length l)
