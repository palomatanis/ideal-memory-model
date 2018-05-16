module TestReplacementPolicies where


import ReplacementPolicies


-- main = do
--     m <- test_complete test_binary
--     appendFile "results/sets/results_binary_kn_o.txt" ((list_to_string m) ++ "\n")
--     --writeFile "results/sets/results_reduction_kn_5.txt" ((list_to_string m) ++ "\n")
--     where list_to_string = unwords . map show


-- test_complete test = do
--   p <- mapM (do_test_of test) [numberAddrToTest_From, (numberAddrToTest_From + (2*associativity))..numberAddrToTest_To]
--   return p
  
-- do_test_of :: (Int -> IO(Int)) -> Int -> IO(Float)
-- do_test_of f n = do
--   p <- replicateM iterations $ f n
--   return (mean p)

  
test pol = do
  l <- pol initialSet (many_consecutive_traces 5 $ consecutive_trace 20)
  putStrLn $ show l


-- test eviction policy for n congruent addresses - Probabilistic replacement policies
testEviction pol n = do
  let (Trace t) = consecutive_trace n
  (s, h) <- pol initialSet (Trace ((SetAddress (n + 1)) : t))
  (s2, Hit h2) <- pol s (Trace [(SetAddress(n + 1))])
  let v = h2 == 0
  return v

  
consecutive_trace :: Int -> Trace
consecutive_trace n = Trace (map SetAddress [1..n])


many_consecutive_traces :: Int -> Trace -> Trace
many_consecutive_traces n (Trace t) = Trace (concat $ replicate n t)

  
-- random_trace :: Int -> IO(Trace)
-- random_trace n = mapM SetAddress $ replicateM n $ randomRIO()
  
