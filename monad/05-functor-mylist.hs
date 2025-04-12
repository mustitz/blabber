module Main where

class MyFunctor f where
    myfmap :: (a -> b) -> f a -> f b

data MyList a = MyEmpty | MyCons a (MyList a)

instance MyFunctor MyList where
  myfmap :: (a -> b) -> MyList a -> MyList b
  myfmap f MyEmpty = MyEmpty
  myfmap f (MyCons x xs) = MyCons (f x) $ myfmap f xs

myshow :: Show a => MyList a -> String
myshow MyEmpty = "MyList[]"
myshow (MyCons x xs) = "MyList[" ++ show x ++ myshowTail xs ++ "]"
  where
    myshowTail MyEmpty = ""
    myshowTail (MyCons y ys) = ", " ++ show y ++ myshowTail ys

toMyList :: [a] -> MyList a
toMyList [] = MyEmpty
toMyList (x:xs) = MyCons x (toMyList xs)

main :: IO ()
main = do
  let a :: MyList Int
      a = MyEmpty

  let b = MyCons 1 $ MyCons 2 $ MyCons 3 MyEmpty
  let c = toMyList ["AAA", "B", "CC"]

  putStrLn $ "A = " ++ myshow a
  putStrLn $ "B = " ++ myshow b
  putStrLn $ "C = " ++ myshow c

  let fa = myfmap (+1) a
  let fb = myfmap (2*) b
  let fc = myfmap length c

  putStrLn $ "fA = " ++ myshow fa
  putStrLn $ "fB = " ++ myshow fb
  putStrLn $ "fC = " ++ myshow fc
