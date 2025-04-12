module Main where

data MyTree a = MyLeaf a
              | MyNode (MyTree a) (MyTree a)
              deriving (Eq, Show)

instance Functor MyTree where
  fmap f (MyLeaf x) = MyLeaf (f x)
  fmap f (MyNode left right) = MyNode (fmap f left) (fmap f right)

instance Applicative MyTree where
  pure = MyLeaf

  (MyLeaf f) <*> (MyLeaf x) = MyLeaf (f x)
  (MyLeaf f) <*> (MyNode left right) = MyNode (MyLeaf f <*> left) (MyLeaf f <*> right)
  (MyNode fl fr) <*> (MyLeaf x) = MyNode (fl <*> MyLeaf x) (fr <*> MyLeaf x)
  (MyNode fl fr) <*> (MyNode l r) = MyNode (fl <*> l) (fr <*> r)

myshow :: Show a => MyTree a -> String
myshow (MyLeaf x) = "MyLeaf " ++ show x
myshow (MyNode left right) =
  "MyNode (" ++ myshow left ++ ") (" ++ myshow right ++ ")"

testLaw name left right = do
  let sleft = myshow left
      sright = myshow right

  if left == right
    then putStrLn $ name ++ " OK"
    else putStrLn $ name ++ " FAIL"

  putStrLn $ "Left:  " ++ sleft
  putStrLn $ "Right: " ++ sright

main = do
  let x = 1
      y = 2
      f = (+10)
      u = MyNode (MyLeaf (+100)) (MyNode (MyLeaf (+200)) (MyLeaf (+300)))
      v = MyNode (MyNode (MyLeaf (+1000)) (MyLeaf (+2000))) (MyLeaf (+3000))
      w = MyNode (MyNode (MyLeaf 10000) (MyLeaf 20000)) (MyNode (MyLeaf 30000) (MyLeaf 40000))

  testLaw "Identity" (pure id <*> w) w
  putStrLn ""

  testLaw "Homomorphism" (pure f <*> pure x) (pure (f x))
  putStrLn ""

  testLaw "Interchange" (u <*> pure y) (pure ($ y) <*> u)
  putStrLn ""

  testLaw "Composition"
          (pure (.) <*> u <*> v <*> w)
          (u <*> (v <*> w))
