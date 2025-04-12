import Prelude hiding ((<$>), (<*>))

data MyMaybe a = MyNothing | MyJust a

class MyFunctor f where
  myfmap :: (a -> b) -> f a -> f b

(<$>) :: MyFunctor f => (a -> b) -> f a -> f b
(<$>) = myfmap

instance MyFunctor MyMaybe where
    myfmap _ MyNothing = MyNothing
    myfmap f (MyJust x) = MyJust (f x)

class MyFunctor f => MyApplicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance MyApplicative MyMaybe where
    pure = MyJust

    MyNothing <*> _ = MyNothing
    (MyJust f) <*> x = myfmap f x

addMaybe2 :: MyMaybe Int -> MyMaybe Int -> MyMaybe Int
addMaybe2 a b = (+) <$> a <*> b

addMaybe3 :: MyMaybe Int -> MyMaybe Int -> MyMaybe Int -> MyMaybe Int
addMaybe3 a b c = add3 <$> a <*> b <*> c
    where add3 a b c = a + b + c

pp :: Show a => MyMaybe a -> String
pp MyNothing = "null"
pp (MyJust x) = show x

test2 :: MyMaybe Int -> MyMaybe Int -> String
test2 a b = "addMaybe2 (" ++ pp a ++ ") (" ++ pp b ++ ") = " ++ pp (addMaybe2 a b)

test3 :: MyMaybe Int -> MyMaybe Int -> MyMaybe Int -> String
test3 a b c = "addMaybe3 (" ++ pp a ++ ") (" ++ pp b ++ ") (" ++ pp c ++ ") = " ++ pp (addMaybe3 a b c)

main = do
    putStrLn $ test2 MyNothing (MyJust 7)
    putStrLn $ test2 (MyJust 5) MyNothing
    putStrLn $ test2 (MyJust 5) (MyJust 7)

    putStrLn $ test3 (MyJust 3) (MyJust 5) (MyJust 7)
    putStrLn $ test3 MyNothing (MyJust 5) (MyJust 7)
    putStrLn $ test3 (MyJust 3) MyNothing (MyJust 7)
    putStrLn $ test3 (MyJust 3) (MyJust 5) MyNothing
    putStrLn $ test3 MyNothing MyNothing MyNothing
