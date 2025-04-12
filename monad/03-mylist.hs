module Main where

data MyList a = MyEmpty | MyCons a (MyList a)

myshow :: Show a => MyList a -> String
myshow MyEmpty = "MyList[]"
myshow (MyCons x xs) = "MyList[" ++ show x ++ myshowTail xs ++ "]"
  where myshowTail MyEmpty = ""
        myshowTail (MyCons y ys) = ", " ++ show y ++ myshowTail ys

myshow1 :: Show a => MyList a -> String
myshow1 MyEmpty = "MyList[]"
myshow1 (MyCons x xs) =
  let myshowTail MyEmpty = ""
      myshowTail (MyCons y ys) = ", " ++ show y ++ myshowTail ys
  in
    "MyList[" ++ show x ++ myshowTail xs ++ "]"

mylist :: [a] -> MyList a
mylist [] = MyEmpty
mylist (x:xs) = MyCons x (mylist xs)

main :: IO ()
main = do
  let a :: MyList Int
      a = MyEmpty

  let b = MyCons 1 $ MyCons 2 $ MyCons 3 MyEmpty
  let c = mylist ["A", "B", "C"]

  putStrLn $ "A = " ++ myshow a
  putStrLn $ "B = " ++ myshow b
  putStrLn $ "C = " ++ myshow c
