module Main where

class MyFunctor f where
    myfmap :: (a -> b) -> f a -> f b

data MyTree a = MyLeaf a
              | MyNode (MyTree a) (MyTree a)

instance MyFunctor MyTree where
  myfmap f (MyLeaf x) = MyLeaf (f x)
  myfmap f (MyNode left right) = MyNode (myfmap f left) (myfmap f right)

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

  let b = MyNode
            (MyNode
              (MyLeaf 1)
              (MyLeaf 2))
            (MyLeaf 3)

  let c = fromlist ["AA", "BBBB", "C", "DDDDD", "EEE"]

  putStrLn $ "A = " ++ myshow a
  putStrLn $ "B = " ++ myshow b
  putStrLn $ "C = " ++ myshow c

  let fa = myfmap (+5) a
  let fb = myfmap (*2) b
  let fc = myfmap length c

  putStrLn $ "fA = " ++ myshow fa
  putStrLn $ "fB = " ++ myshow fb
  putStrLn $ "fC = " ++ myshow fc
