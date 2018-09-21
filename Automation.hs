module Main where


import System.IO

import Base
import Cache_model
import Tests

cfrom = 1
cto = 6
dfrom = 1
dto = 6
lfrom = 1
lto = 6

-- lru bip
-- test_adaptive_eviction lru bip (1, 2, 1)

main = do
  let eviction_strategies = filter (\(_, b, c) -> b >= c) $ [ (x,y,z) | x<-[cfrom..cto], y<-[dfrom..dto], z<-[lfrom..lto] ]
  mapM (\ev@(a, b, c) -> executeTestAdaptive ("./adaptive/adaptive_eviction_test_50it_512psel_lru_bip_" ++ (show a) ++ "_" ++ (show b) ++ "_" ++ (show c)) lru bip ev) eviction_strategies


saveArr = do
    outh <- openFile "test.txt" WriteMode
    mapM (hPutStrLn outh . show) [1,2,3]
--     hPrint outh [1,2,3]
    hClose outh
