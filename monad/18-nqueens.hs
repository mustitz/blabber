module Main where

import Control.Monad

queens n = foldM (\y _ -> [ x : y | x <- [1..n], safe x y 1]) [] [1..n] where
    safe x [] _ = True
    safe x (c:y) n = and [ x /= c , x /= c + n , x /= c - n , safe x y (n+1)]

main = do
  print $ length $ queens 8
