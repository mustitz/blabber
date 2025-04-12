safeSqrt :: Double -> Maybe Double
safeSqrt x | x >= 0    = Just $ sqrt x
           | otherwise = Nothing

safeLog :: Double -> Maybe Double
safeLog x | x > 0     = Just $ log x
          | otherwise = Nothing

logThenSqrt :: Double -> Maybe Double
logThenSqrt x = pure x >>= safeLog >>= safeSqrt

main :: IO ()
main = do
  putStrLn $ "logThenSqrt (-5) = " ++ show (logThenSqrt (-5))
  putStrLn $ "logThenSqrt (0.5) = " ++ show (logThenSqrt (0.5))
  putStrLn $ "logThenSqrt (7) = " ++ show (logThenSqrt (7))
