import System.Environment
import Data.Random
import Data.List
import Data.Char

test_trace policy hh m ll t = do
  let l = map (\x -> (x, '?')) ll
  let set = CacheSetContent $ l
  putStrLn $ "\n\ \Trace: " ++ (show t) ++  " \n\ \ "
  putStrLn $ "Initial Cache Set: " ++ (show l) ++ " \n\ \ "
  (CacheSetContent csc, Hit h, Hit m) <- policy (if (hh == "hp") then True else False) set (Trace t) m
  let evicted = (length l) - (length $ filter (\(a,b) -> b == '?') csc)
  putStrLn $ (show h) ++ " hits, " ++ (show m) ++ " misses, " ++ (show evicted) ++ " evicted  \n\ \ "

test_pattern policy hh m ll (a,b,c,d) = do
  let t = ([(a,b,c,d,1)],1)
  r <- test_pattern_long policy hh m ll t
  return r
  
test_pattern_long policy hh m ll p = do
  let t = generate_trace p
  r <- test_trace policy hh m ll t
  return r

generate_trace :: EvictionStrategyExtra -> [Char]
generate_trace e = generate_trace' e [] 1
  where
    generate_trace' :: EvictionStrategyExtra -> [Char] -> Int -> [Char]
    generate_trace' ([], rep) lis _ = concat $ replicate rep lis
    generate_trace' (((c,d,l,s,repL):es), repG) lis n = generate_trace' (es, repG) (lis ++ trace) (n+s)
      where
        alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
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
          
bip_probability = 2


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
            putStrLn $ (show h) ++ " -> Hit: " ++ (show new_cache_st) ++ " \n\ \ "
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
                putStrLn $ (show h) ++" -> Miss, introduce: " ++ (show new_cache_st) ++ " \n\ \ "
                rrip'(Trace $ tail trace, CacheSetContent new_cache_st, Hit hit, Hit $ misses + 1) mbrrip hp isbrrip
              Nothing -> do
                let new_cache_st = map (\(a,b) -> (a+1,b)) set
                putStrLn $ "Miss, increase: " ++ (show new_cache_st) ++ " \n\ \ "
                rrip'(Trace trace, CacheSetContent new_cache_st, Hit hit, Hit misses) mbrrip hp isbrrip
        where h = head trace      

-- initialSetPLRU :: IO(CacheSetContent)
-- initialSetPLRU = do
--   l <- replicateM associativity $ randomRIO(0, 1)
--   let r = map (\x -> (x, AddressIdentifier 0)) l
--   return $ CacheSetContent r

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
