module Main where

data MyTree a = MyLeaf a
              | MyNode (MyTree a) (MyTree a)

myshow :: Show a => MyTree a -> String
myshow (MyLeaf x) = "MyLeaf " ++ show x
myshow (MyNode left right) =
  "MyNode (" ++ myshow left ++ ") (" ++ myshow right ++ ")"

fromlist :: [a] -> MyTree a
fromlist [] = error "fromList: empty list"
fromlist [x] = MyLeaf x
fromlist xs =
  let (left, right) = splitAt (length xs `div` 2) xs
  in MyNode (fromlist left) (fromlist right)

main = do
  let a = MyLeaf 0

      b = MyNode
            (MyNode
              (MyLeaf 1)
              (MyLeaf 2))
            (MyLeaf 3)

      c = fromlist ["A", "B", "C", "D", "E"]

  putStrLn $ "A = " ++ myshow a
  putStrLn $ "B = " ++ myshow b
  putStrLn $ "C = " ++ myshow c
