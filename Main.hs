module Main where

import System.Random
import Address_translation
import Control.Monad


-- media de 1000 repeticiones
-- entre 0 y 4000 direcciones. intervalo asociatividad
numberAddrToTest_From :: Int
numberAddrToTest_From = 0

numberAddrToTest_To :: Int
numberAddrToTest_To = 400

numberAddrToTest :: Int
numberAddrToTest = 400

iterations :: Int
iterations = 10

main = do
  p <- mapM test_n [numberAddrToTest_From, (numberAddrToTest_From + associativity)..numberAddrToTest_To]
  putStrLn $ show p

  
-- Mean of eviction sets for n addresses
test_n :: Int -> IO(Float)  
test_n n = do
  p <- replicateM iterations (count_evictions n)
  return (mean p)

-- Mean of a list
mean :: [Int] -> Float
mean l = (fromIntegral $ sum l)/(fromIntegral $ length l)
          
-- Creates random list of addresses, and counts eviction sets  
count_evictions :: Int -> IO(Int)
count_evictions number = do
  p <- listOfRandomAddr number
  return (number_of_eviction_sets $ separateAdressesIntoBins p)
  -- putStr $ toStringAddresses p


-- Creates random list of virtual addresses, transforms to phyisical, says how many sets have 'associativity' addresses
calculate_evictions :: IO()
calculate_evictions = do
  p <- listOfRandomAddr numberAddrToTest
  listR <- generateSeeds numberAddrToTest
  putStrLn $ print_evictions $ map virtual_to_physical_translation $ zip p listR


-- Given a set of addresses, prints info on eviction sets
print_evictions :: [Address] -> String
print_evictions addr = "Out of " ++ (show (2^cacheSetBits)) ++ " sets, there are " ++ (show (number_of_eviction_sets $ separateAdressesIntoBins addr)) ++ " eviction sets"


-- create list of n random addresses (virtual)
listOfRandomAddr :: Int -> IO ([Address])
listOfRandomAddr 0 = return []
listOfRandomAddr n = do
  r <- randomAddr virtualAddressLength
  rs <- listOfRandomAddr (n-1)
  return (r:rs)


-- Create random address of length n
randomAddr :: Int -> IO (Address)
randomAddr n = do
  newRand <- randomIO --  :: IO Int
  return (createRandom_Address (newRand, n))

-- Generates a list of n seeeds
generateSeeds :: Int -> IO ([Int])
generateSeeds 0 = return []
generateSeeds n = do
  r <- randomIO
  rs <- generateSeeds (n-1)
  return (r:rs)
