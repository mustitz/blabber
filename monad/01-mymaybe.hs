module Main where

data MyMaybe a = MyNothing | MyJust a

myshow1 :: Show a => MyMaybe a -> String
myshow1 val = case val of
  MyNothing -> "MyNothing"
  MyJust x  -> "MyJust: " ++ show x

myshow :: Show a => MyMaybe a -> String
myshow MyNothing = "MyNothing"
myshow (MyJust x) = "MyJust: " ++ show x

main = do
  let a :: MyMaybe Int
      a = MyNothing
      b = MyJust 123

  putStrLn $ "A = " ++ myshow a
  putStrLn $ "B = " ++ myshow b
