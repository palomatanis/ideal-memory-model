module Main where

import System.Random
import Address_translation


numberAddrToTest :: Int
numberAddrToTest = 100
  


-- main = do randomList <- listOfRandomAddr numberAddrToTest
--           putStrLn $ checkEvictionSet $ separateAdressesIntoBins $ map (createRandom_PhysicalFromVirtual) randomList

main = do
  p <- listOfRandomAddr numberAddrToTest
  putStr $ toStringAddresses p


-- Creates random list of virtual addresses, transforms to phyisical, says how many sets have 'associativity' addresses
printEvictions = do
  p <- listOfRandomAddr numberAddrToTest
  listR <- generateSeeds numberAddrToTest
  putStrLn $ evictions $ map createRandom_PhysicalFromVirtual $ zip p listR

-- Given a set of addresses, prints info on eviction sets
evictions :: [Address] -> String
evictions addr = "Out of " ++ (show (2^setBits)) ++ " sets, there are " ++ (show (checkEvictionSet $ separateAdressesIntoBins addr)) ++ " eviction sets"


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
