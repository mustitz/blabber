module Main where

data MyList a = MyEmpty | MyCons a (MyList a) deriving Show

myappend :: MyList a -> MyList a -> MyList a
myappend MyEmpty ys = ys
myappend (MyCons x xs) ys = MyCons x (myappend xs ys)

instance Functor MyList where
  fmap :: (a -> b) -> MyList a -> MyList b
  fmap f MyEmpty = MyEmpty
  fmap f (MyCons x xs) = MyCons (f x) $ fmap f xs

instance Applicative MyList where
    pure x = MyCons x MyEmpty
    MyEmpty <*> _ = MyEmpty
    (MyCons f fs) <*> xs = myappend (fmap f xs) (fs <*> xs)

instance Monad MyList where
  MyEmpty >>= _ = MyEmpty
  (MyCons x xs) >>= f = myappend (f x) (xs >>= f)

boardSize :: Int
boardSize = 5

allRanks :: MyList Int
allRanks = range boardSize
   where range 0 = MyEmpty
         range n = MyCons n $ range $ n-1

type Square = (Int, Int)
type Queens = MyList Square

connected :: Square -> Square -> Bool
connected (file1, rank1) (file2, rank2) = sameFile || sameRank || sameDiagonal
  where sameFile = file1 == file2
        sameRank = rank1 == rank2
        sameDiagonal = abs (file1 - file2) == abs (rank1 - rank2)

unsafe :: Square -> Queens -> Bool
unsafe _ MyEmpty = False
unsafe square (MyCons head tail) = (connected square head) || unsafe square tail

addQueen :: Int -> Queens -> MyList Queens
addQueen file existingQueens = do
    rank <- allRanks
    if unsafe (file, rank) existingQueens
        then MyEmpty
        else return (MyCons (file, rank) existingQueens)

queens :: Int -> MyList Queens
queens 0 = return MyEmpty
queens n = do
    qs <- queens (n-1)
    addQueen n qs

printSolutions :: MyList Queens -> IO ()
printSolutions MyEmpty = return ()
printSolutions (MyCons head tail) = do
    putStrLn $ show head
    printSolutions tail

main :: IO ()
main = do
    putStrLn $ show boardSize ++ "-Queens solutions:"
    printSolutions $ queens boardSize
