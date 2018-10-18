import System.Environment
import System.Random
import Data.Random
import Data.List
import Data.Char
import Control.Monad

import Numeric
import Data.Maybe

prints = True
printsAll = False
save = False

take_pattern = 20
length_trace = 10

bip_probability = 2 -- over 64

path = "./adaptive/srrip_initial/plru_tests_initial_states_traces_"

printToFileLn :: String -> String -> IO()
printToFileLn path_end text = do
  appendFile (path ++ path_end) text


patterns = map (\(c, d, l, n, r) -> ([(c,d,l,n,r)],1)) $ [ (c,d,l,n,r) | c <- [1,2,3], d <- [1, 3, 4], l <- [1,3], n <- [4], r <- [1,2], (l < d || l == 1) ]

traces = ["aabbcdbdca", "abccddcaab", "abbccdcdba", "abbccdbcda", "abccddcbba", "aabccdcadb", "aabccddcab", "abccddaacb", "aabccddbca", "aabccdadcb", "abbccddbca", "aabccdcdab", "abbcddbaac", "abbccddacb", "aabbccdacb", "abbcddbcca", "abbccdbdac", "aabbccdcba", "abccddacab", "abbccddbac", "abbccdcbda", "aabbcdbdac", "abccddbbca", "abccddbbac", "aabccdacdb", "aabbcddbca", "abbcdbdaac", "aabbccdbca", "aabccdadbc", "aabbcdbadc", "aabccdcdba", "aabbccdbac", "aabbccdcab", "abbccddcab", "aabbcddbac", "abccdcdbba", "abbccdcdab", "abbcdbdcca", "abccddbcba", "aabccddacb", "abbccddcba", "aabccddcba", "aabbcddcba", "abbccdbdca", "abccdcdaab", "aabccddabc"]

-- -- All patterns removing equivalences (aaabb = bbbaa == cccdd)
-- traces = rmdups $ map trans $ replicateM length_trace ['a', 'b', 'c', 'd']
--   where
--     trans tra = map (\x -> if (x == nubbed !! 0) then 'a' else if (x == nubbed !! 1) then 'b' else if (x == nubbed !!2) then 'c' else 'd') tra
--       where nubbed = nub tra
-- rmdups = map head . group . sort


test_of_tests_pattern policy hh m assoc = do
  mapM (\x -> test_all_pattern policy hh m assoc x) patterns

test_of_tests_traces policy hh m assoc = do
  mapM (\x -> test_all_trace policy hh m assoc x) traces
  
cr_name ([(c,d,l,n,r)], r2) = "_" ++ (show c) ++ "_" ++ (show d) ++ "_" ++ (show l) ++ "_" ++ (show n) ++ "_" ++ (show r)

test_all_trace policy hh m assoc trace = do
  let file_name = show trace
  -- when save $ printToFileLn $ "\n\ \Trace: " ++ (show trace) ++  " \n\ \ "
  let initial_states = allCombinations assoc m
  r <- mapM (\x -> test_trace file_name policy hh m x trace) initial_states
  let (h,m,ev) = unzip3 r
  let total = length $ filter (== assoc) ev
  let ma = if (m == []) then 0 else maximum m
  let pr2 = "\n\ \Totally evicted " ++ (show $ length $ filter (== assoc) ev) ++ " out of " ++ (show $ length r) ++ ", max misses = " ++ (show $ maximum m) ++ "\n\ \"
  when (total == (length $ initial_states)) $ do
    when save $ writeFile (path ++ file_name) ""
    let pr = (show total) ++ " " ++ (show ma) ++ "\n\ \"
    when save $ printToFileLn file_name pr
  when prints $ putStrLn pr2

test_all_pattern policy hh m assoc trace = do
  let file_name = cr_name trace
  writeFile (path ++ file_name) ""
  let initial_states = allCombinations assoc m
  r <- mapM (\x -> test_pattern_long file_name policy hh m x trace) initial_states
  let (h,m,ev) = unzip3 r
  let pr = (show $ length $ filter (== assoc) ev) ++ " " ++ (show $ maximum m) ++ "\n\ \"
--  let pr = "\n\ \Totally evicted " ++ (show $ length $ filter (== assoc) ev) ++ " out of " ++ (show $ length r) ++ ", max misses = " ++ (show $ maximum m) ++ "\n\ \"
  when save $ printToFileLn file_name pr
  
allCombinations :: Int -> Int -> [[Int]]
allCombinations assoc m = replicateM assoc [0..(2^m-1)]
  
test_trace fn policy hh m ll t = do
  (l, as) <- do
    case ll of
      [a] -> do
        la <- initialSet m a
        return $ (CacheSetContent $ la, a)
      _ -> do
        let la = map (\x -> (x, '?')) ll
        return $ (CacheSetContent $ la, length ll)
  when printsAll $ putStrLn $ "\n\ \Trace: " ++ (show t) ++  " \n\ \ "
  -- when save $ printToFileLn $ "\n\ \Trace: " ++ (show trace) ++  " \n\ \ "
  when printsAll $ putStrLn $ "Initial Cache Set: " ++ (show l) ++ " \n\ \ "
  -- when save $ printToFileLn $ "\n\ \Initial Cache Set: " ++ (show l) ++ " \n\ \ " 
  (CacheSetContent csc, Hit h, Hit m) <- policy (if (hh == "hp") then True else False) l (Trace t) m
  let evicted = as - (length $ filter (\(a,b) -> b == '?') csc)
  when printsAll $ putStrLn $ (show h) ++ " hits, " ++ (show m) ++ " misses, " ++ (show evicted) ++ " evicted  \n\ \ "
  -- when save $ printToFileLn fn $ (show h) ++ " " ++ (show m) ++ " " ++ (show evicted) ++ "\n\ \ "
--   when save $ printToFileLn fn $ (show h) ++ " hits, " ++ (show m) ++ " misses, " ++ (show evicted) ++ " evicted  \n\ \ "
  return (h,m,evicted)

test_pattern fp policy hh m ll (a,b,c,d) = do
  let t = ([(a,b,c,d,1)],1)
  r <- test_pattern_long fp policy hh m ll t
  return r
  
test_pattern_long fn policy hh m ll p = do
  let t = generate_trace p
  r <- test_trace fn policy hh m ll t 
  return r
  
generate_trace :: EvictionStrategyExtra -> [Char]
generate_trace e = generate_trace' e [] 1
  where
    generate_trace' :: EvictionStrategyExtra -> [Char] -> Int -> [Char]
--    generate_trace' ([], rep) lis _ = concat $ replicate rep lis
    generate_trace' ([], rep) lis _ = take take_pattern $ cycle $ concat $ replicate rep lis
    generate_trace' (((c,d,l,s,repL):es), repG) lis n = generate_trace' (es, repG) (lis ++ trace) (n+s)
      where
        alphabet = cycle "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        set = map (\x -> alphabet !! (x - 1))([n..(n+s-1)])
        tr = eviction_strategy_trace set (c,d,l)
        trace = concat $ replicate repL tr

-- Creates the set of addresses to test eviction with a certain eviction strategy
eviction_strategy_trace :: [Char] -> EvictionStrategy -> [Char]
eviction_strategy_trace set strategy = map (\x -> set !! x) (eviction_strategy_trace' set strategy 0)
  where
    eviction_strategy_trace' :: [Char] -> EvictionStrategy -> Int -> [Int]
    eviction_strategy_trace' [] (c, d, l) n = []
    eviction_strategy_trace' set (c, d, l) n = group ++ recursion
      where
        group = (take (c * d) $ cycle [n..(n+d-1)])
        recursion = if ((n + l) > ((length set) - d))
          then []
          else eviction_strategy_trace' set (c, d, l) (n + l)


srrip :: Bool -> CacheSetContent -> Trace -> Int -> IO(CacheSetContent, HitNumber, HitNumber)
srrip hp set trace m = do
  r <- rrip False hp set trace m
  return r
  
brrip :: Bool -> CacheSetContent -> Trace -> Int -> IO(CacheSetContent, HitNumber, HitNumber)
brrip hp set trace m = do
  r <- rrip True hp set trace m
  return r

rrip :: Bool -> Bool -> CacheSetContent -> Trace -> Int -> IO(CacheSetContent, HitNumber, HitNumber)
rrip isbrrip hp set trace mbrrip = do
  (t, s, h, m) <- rrip'(trace, set, Hit 0, Hit 0) mbrrip hp isbrrip
  return (s, h, m)
    where
      rrip' :: (Trace, CacheSetContent, HitNumber, HitNumber) -> Int -> Bool -> Bool -> IO(Trace, CacheSetContent, HitNumber, HitNumber)
      rrip' i@(Trace [], a, b, c) _ _ _ = do return i
      rrip' (Trace trace, CacheSetContent set, Hit hit, Hit misses) mbrrip hp isbrrip = do
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            let (reg, el) = set !! elem
            let new_cache_st = take elem set ++ [((if hp then 0 else (if reg > 0 then reg-1 else 0)), el)] ++ drop (elem + 1) set
            when printsAll $ putStrLn $ (show h) ++ " -> Hit: " ++ (show new_cache_st) ++ " \n\ \ "
            rrip'(Trace (tail trace), CacheSetContent(new_cache_st), Hit $ hit + 1, Hit misses) mbrrip hp isbrrip
          Nothing ->
            case (elemIndex (2^mbrrip-1) $ map (\(a,b) -> a) set) of
              Just elem -> do -- There is a rrpv register with a (2^m-1)
                let (reg, el) = set !! elem
                c <- chance64 bip_probability
                let b = if isbrrip then c else True
                let new_cache_st = if b
                      then (take elem set ++ [(2^mbrrip-2, h)] ++ drop (elem + 1) set)
                      else (take elem set ++ [(2^mbrrip-1, h)] ++ drop (elem + 1) set)
                when printsAll $ putStrLn $ (show h) ++" -> Miss, introduce: " ++ (show new_cache_st) ++ " \n\ \ "
                rrip'(Trace $ tail trace, CacheSetContent new_cache_st, Hit hit, Hit $ misses + 1) mbrrip hp isbrrip
              Nothing -> do
                let new_cache_st = map (\(a,b) -> (a+1,b)) set
                when printsAll $ putStrLn $ "Miss, increase: " ++ (show new_cache_st) ++ " \n\ \ "
                rrip'(Trace trace, CacheSetContent new_cache_st, Hit hit, Hit misses) mbrrip hp isbrrip
        where h = head trace

-- First in first out
fifo :: Bool -> CacheSetContent -> Trace -> Int -> IO(CacheSetContent, HitNumber, HitNumber)
fifo _ set trace _ = do
  (t, s, h, m) <- fifo'(trace, set, Hit 0, Hit 0)
  return (s, h, m)
    where
      fifo' :: (Trace, CacheSetContent, HitNumber, HitNumber) -> IO(Trace, CacheSetContent, HitNumber, HitNumber)
      fifo' i@(Trace [], _, _, _) = do return i
      fifo' (Trace trace, CacheSetContent set, Hit hit, Hit misses) =
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just _ -> do
            when printsAll $ putStrLn $ (show h) ++ " -> Hit: " ++ (show set) ++ " \n\ \ "
            fifo'(Trace $ tail trace, CacheSetContent set, Hit $ hit + 1, Hit misses)
          Nothing -> do
            let new_cache_st = (0,h) : init set
            when printsAll $ putStrLn $ (show h) ++" -> Miss, introduce: " ++ (show new_cache_st) ++ " \n\ \ "
            fifo'(Trace $ tail trace, CacheSetContent new_cache_st, Hit hit, Hit $ misses + 1)
        where h = head trace


 -- Least recently used
lru :: Bool -> CacheSetContent -> Trace -> Int -> IO(CacheSetContent, HitNumber, HitNumber)
lru b set trace m = do
  r <- ru False set trace
  return r
  
-- Bimodal insertion policy
bip :: Bool -> CacheSetContent -> Trace -> Int -> IO(CacheSetContent, HitNumber, HitNumber)
bip b set trace bip_probability = do
  r <- ru True set trace
  return r

-- Bimodal insertion policy
ru :: Bool -> CacheSetContent -> Trace -> IO(CacheSetContent, HitNumber, HitNumber)
ru isbip set trace = do
  (t, s, h, m) <- ru'(trace, set, Hit 0, Hit 0) isbip
  return (s, h, m)
    where
      ru' :: (Trace, CacheSetContent, HitNumber, HitNumber) -> Bool -> IO(Trace, CacheSetContent, HitNumber, HitNumber)
      ru' i@(Trace [], _, _, _) _ = do return i
      ru' (Trace trace, CacheSetContent set, Hit hit, Hit misses) isbip = 
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            let (reg,_) = set !! elem
            let new_cache_st = if (elem == ((length set) - 1)) then ((reg,h) : (init set)) else set
            r <- ru' (Trace (tail trace), CacheSetContent new_cache_st, Hit $ hit + 1, Hit misses) isbip
            when printsAll $ putStrLn $ (show h) ++ " -> Hit: " ++ (show new_cache_st) ++ " \n\ \ "
            return r
          Nothing -> do
            c <- chance64 bip_probability
            let b = if isbip then c else True
            let new_cache_st = if b
                  then ((fst $ head set,h) : init set)
                  else (init set ++ [(fst $ last set,h)])
            when printsAll $ putStrLn $ (show h) ++" -> Miss, introduce: " ++ (show new_cache_st) ++ " \n\ \ "
            ru'(Trace $ tail trace, CacheSetContent new_cache_st, Hit hit, Hit $ misses + 1) isbip
        where h = head trace


 -- Least recently used
plru :: Bool -> CacheSetContent -> Trace -> Int -> IO(CacheSetContent, HitNumber, HitNumber)
plru _ set trace _ = do
  (t, s, h, m) <- plru'(trace, set, Hit 0, Hit 0)
  return (s, h, m)
    where
      plru' :: (Trace, CacheSetContent, HitNumber, HitNumber) -> IO(Trace, CacheSetContent, HitNumber, HitNumber)
      plru' i@(Trace [], _, _, _) = do return i
      plru' (Trace trace, CacheSetContent set, Hit hit, Hit misses) =
        case (elemIndex h $ map (\(a,b) -> b) set) of
          Just elem -> do
            let assoc = length set
            new_set <- plru_find elem set assoc
            when printsAll $ putStrLn $ (show h) ++ " -> Hit: " ++ (show new_set) ++ " \n\ \ "
            plru'(Trace $ tail trace, CacheSetContent new_set, Hit $ hit + 1, Hit misses)
          Nothing -> do
            let assoc = length set
            (up_set, elem) <- plru_insert set assoc
            let (reg,_) = up_set !! elem
            let new_set = take elem up_set ++ [(reg, h)] ++ drop (elem + 1) up_set
            when printsAll $ putStrLn $ (show h) ++" -> Miss, introduce: " ++ (show new_set) ++ " \n\ \ "
            plru'(Trace $ tail trace, CacheSetContent new_set, Hit hit, Hit $ misses + 1)
        where h = head trace

        
plru_insert :: [(Int, Char)] -> Int -> IO(([(Int, Char)], Int))
plru_insert set associativity = plru_insert' (associativity `div` 2) set ""
  where
    plru_insert' :: Int -> [(Int, Char)] -> String -> IO(([(Int, Char)], Int))
    plru_insert' 0 set list = do
      let r = readBin list
      return (set, r)
    plru_insert' tier set l = do
      b <- randomRIO(0, 1)
      let bin = if (a == 2) then b else a
      if (bin == 0)
        then do
        (ret, r) <- plru_insert' (if (tier == 1) then 0 else tier `div` 2) (drop 1 set) (l++(show 0))
        return (((if (a == 2) then a else 1), val) : ret, r)
        else do
        (ret, r) <- plru_insert' (if (tier == 1) then 0 else tier `div` 2) (drop tier set) (l++(show 1))
        return (((if (a == 2) then a else 0), val):(take (tier - 1) $ tail set) ++ ret, r)
          where
            (a, val) = head set


plru_find :: Int -> [(Int, Char)] -> Int -> IO([(Int, Char)])
plru_find address set associativity = plru_find' (associativity `div` 2) set (to_bin address associativity)
  where
    plru_find' :: Int -> [(Int, Char)] -> [Int] -> IO([(Int, Char)])
    plru_find' 0 set list = do return set
    plru_find' tier set (l:ls) = do
      if (l == 0)
        then do
        ret <- plru_find' (if (tier == 1) then 0 else tier `div` 2) (drop 1 set) ls
        return (((if (a == 2) then a else 1), val) : ret)
        else do
        ret <- plru_find' (if (tier == 1) then 0 else tier `div` 2) (drop tier set) ls
        return (((if (a == 2) then a else 0), val):(take (tier - 1) $ tail set) ++ ret)
          where
            (a, val) = head set


to_bin :: Int -> Int -> [Int]
to_bin n associativity = to_length_bin $ to_bin' n
  where
    to_bin' :: Int -> [Int]
    to_bin' 0 = []
    to_bin' n | n `mod` 2 == 1 = to_bin' (n `div` 2) ++ [1]
              | n `mod` 2 == 0 = to_bin' (n `div` 2) ++ [0]
    to_length_bin :: [Int] -> [Int]
    to_length_bin l = if ((length l) == (truncate $ logBase (fromIntegral 2) (fromIntegral associativity))) then l else to_length_bin (0:l)

    
readBin :: String -> Int
readBin = fromMaybe 0 . toBin
  where toBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt


  
-- Adaptive replacement policy
-- type AdaptiveRepPol = SetAddresses -> IO(CacheSetContent, HitNumber)

initialSet :: Int -> Int -> IO([(Int, Char)])
initialSet m associativity = do
  l <- replicateM associativity $ randomRIO(0, 2^m-1)
  let r = map (\x -> (x, '?')) l
  return r

initialSetPLRU :: Int -> IO([(Int, Char)])
initialSetPLRU associativity = do
  l <- replicateM associativity $ randomRIO(0, 1)
  let r = map (\x -> (x, '?')) l
  return r


-- List of congruent addresses that are going to be inserted to a cache
newtype Trace = Trace String
  deriving (Read, Show, Eq)

-- Content of a cache set, represented as the list of addresses inside it. There is a register per slot
newtype CacheSetContent = CacheSetContent [(Int, Char)]
  deriving (Read, Show, Eq)

-- Number of hits after a trace is put inside a cache
newtype HitNumber = Hit Int
  deriving (Read, Show, Eq)

type EvictionStrategy = (Int, Int, Int)

-- ([(c,d,l,s,rep),(c',d',l',s',rep')], reptot)
type EvictionStrategyExtra = ([(Int, Int, Int, Int, Int)], Int)


-- Receives probability (out of 64) and throws a coin with that prob
chance64 :: Int -> IO(Bool)
chance64 nu = do
  let distr = (take nu $ repeat True) ++ (take (64 - nu) $ repeat False)
  r <- sample $ randomElement distr
  return r
