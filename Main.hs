module Main where

import System.Random
import Address_translation
import Control.Monad
import System.IO

-- media de 1000 repeticiones
-- entre 0 y 4000 direcciones. intervalo asociatividad
numberAddrToTest_From :: Int
numberAddrToTest_From = 0

numberAddrToTest_To :: Int
numberAddrToTest_To = 4000

iterations :: Int
iterations = 100

-- Save tests
main = do
    file <- openFile "results.txt" WriteMode
    m <- test_complete
    hPrint file m
    hClose file

    
test_complete = do
  p <- mapM test_n_addresses [numberAddrToTest_From, (numberAddrToTest_From + (2*associativity))..numberAddrToTest_To]
  return p
  
-- Mean of eviction sets for n addresses
test_n_addresses :: Int -> IO(Float)  
test_n_addresses n = do
  p <- replicateM iterations (count_evictions n)
  return (mean p)

         
-- Creates random list of n physical addresses from a virtual address, and counts eviction sets  
count_evictions :: Int -> IO(Int)
count_evictions number = do
  listR <- generateSeeds number
  return (number_of_eviction_sets $ map virtual_to_physical_translation $ zip (listOfNotRandomAddr number) listR)
  

-- create list of n [0...0] addresses (virtual)
listOfNotRandomAddr :: Int -> [Address]
listOfNotRandomAddr 0 = []
listOfNotRandomAddr n = (createAddress $ take virtualAddressLength [0,0..0]):listOfNotRandomAddr (n-1)


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
