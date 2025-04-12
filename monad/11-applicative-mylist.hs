import Prelude hiding ((<$>), (<*>))

class MyFunctor f where
    myfmap :: (a -> b) -> f a -> f b

class MyFunctor f => MyApplicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

data MyList a = MyEmpty | MyCons a (MyList a)

instance MyFunctor MyList where
    myfmap _ MyEmpty = MyEmpty
    myfmap f (MyCons x xs) = MyCons (f x) (myfmap f xs)

(<$>) :: MyFunctor f => (a -> b) -> f a -> f b
(<$>) = myfmap

instance MyApplicative MyList where
    pure x = MyCons x MyEmpty

    MyEmpty <*> _ = MyEmpty
    (MyCons f fs) <*> xs = myappend (myfmap f xs) (fs <*> xs)
      where
        myappend MyEmpty ys = ys
        myappend (MyCons x xs) ys = MyCons x (myappend xs ys)

myshow :: Show a => MyList a -> String
myshow MyEmpty = "MyList[]"
myshow (MyCons x xs) = "MyList[" ++ show x ++ myshowTail xs ++ "]"
  where
    myshowTail MyEmpty = ""
    myshowTail (MyCons y ys) = ", " ++ show y ++ myshowTail ys

mylist :: [a] -> MyList a
mylist [] = MyEmpty
mylist (x:xs) = MyCons x (mylist xs)

main :: IO ()
main = do
    let list1 :: MyList (Int -> Int)
        list1 = mylist [ (+7), (3*) ]
    let list2 = mylist [ 1, 2, 3, 4, 5 ]
    let list3 = list1 <*> list2
    putStrLn $ "list2 = " ++ myshow list2
    putStrLn $ "list3 = " ++ myshow list3
