module Main where

data MyTree a = MyLeaf a
              | MyNode (MyTree a) (MyTree a)
              deriving (Show)

instance Functor MyTree where
  fmap f (MyLeaf x) = MyLeaf (f x)
  fmap f (MyNode x y) = MyNode (fmap f x) (fmap f y)

instance Applicative MyTree where
  pure = MyLeaf

  (MyLeaf f) <*> (MyLeaf x) = MyLeaf (f x)
  (MyLeaf f) <*> (MyNode left right) = MyNode (MyLeaf f <*> left) (MyLeaf f <*> right)
  (MyNode fl fr) <*> (MyLeaf x) = MyNode (fl <*> MyLeaf x) (fr <*> MyLeaf x)
  (MyNode fl fr) <*> (MyNode l r) = MyNode (fl <*> l) (fr <*> r)

instance Monad MyTree where
  (MyLeaf x) >>= f = f x
  (MyNode x y) >>= f = MyNode (x >>= f) (y >>= f)

data Player = Player1 | Player2 deriving Show
data GameState = Win Player | InProgress (Player, Int) deriving Show

next :: Player -> Player
next Player1 = Player2
next Player2 = Player1

take1 :: (Player, Int) -> GameState
take1 (player, n)
  | n > 1 = InProgress (next player, n-1)
  | otherwise = Win (next player)

take2 :: (Player, Int) -> GameState
take2 (player, n)
  | n > 2 = InProgress (next player, n-2)
  | otherwise = Win (next player)

grow :: GameState -> MyTree GameState
grow (Win player) = MyLeaf (Win player)
grow (InProgress state) = MyNode (MyLeaf (take1 state)) (MyLeaf (take2 state))

grow3 :: GameState -> MyTree GameState
grow3 state0 = do
  state1 <- grow state0
  state2 <- grow state1
  grow state2

main = do
  let initial = InProgress (Player1, 4)
  putStrLn $ show (grow3 initial)
