module Main where

data MyMaybe a = MyJust a | MyNothing deriving Show

instance Functor MyMaybe where
  fmap _ MyNothing = MyNothing
  fmap f (MyJust x) = MyJust (f x)

instance Applicative MyMaybe where
   pure = MyJust
   MyNothing <*> _ = MyNothing
   (MyJust f) <*> something = fmap f something

instance Monad MyMaybe where
  MyNothing >>= _ = MyNothing
  (MyJust x) >>= f = f x

myguard :: Bool -> MyMaybe ()
myguard True = MyJust ()
myguard False = MyNothing

type UID = Int
type Price = Double
data Product = Product { productUid :: UID, amount :: Int, price :: Price } deriving Show
data User = User { userUid :: UID, balance :: Double } deriving Show

getProduct :: UID -> MyMaybe Product
getProduct 105 = MyJust $ Product { productUid = 105, amount = 3, price = 9.90 }
getProduct 107 = MyJust $ Product { productUid = 107, amount = 7, price = 4.30 }
getProduct _ = MyNothing

getPrice :: UID -> MyMaybe Price
getPrice uid = do
  product <- getProduct uid
  return $ price product

getPrice' :: UID -> MyMaybe Price
getPrice' uid = getProduct uid >>= \product -> return (price product)

getPrice'' :: UID -> MyMaybe Price
getPrice'' uid = price <$> getProduct uid

getStock :: UID -> MyMaybe Int
getStock uid = do
  product <- getProduct uid
  return $ amount product

checkAmount :: Int -> MyMaybe ()
checkAmount amount = myguard $ amount > 0

limitToStock :: UID -> Int -> MyMaybe Int
limitToStock productId amount = do
    avail <- getStock productId
    return $ min amount avail

debet :: User -> Price -> MyMaybe User
debet user price
  | balance user >= price = MyJust $ user { balance = balance user - price }
  | otherwise = MyNothing

buyMaximum :: User -> UID -> Int -> MyMaybe User
buyMaximum user productId amount = do
    checkAmount amount
    left <- limitToStock productId amount
    checkAmount left
    price <- getPrice productId
    let money = price * fromIntegral left
    debet user money

buyMaximum' :: User -> UID -> Int -> MyMaybe User
buyMaximum' user productId amount =
    checkAmount amount >>= \_ ->
    limitToStock productId amount >>= \left ->
    checkAmount left >>= \_ ->
    getPrice productId >>= \price ->
    let money = price * fromIntegral left
    in debet user money

buyMaximum'' :: User -> UID -> Int -> MyMaybe User
buyMaximum'' user productId amount =
    (checkAmount amount >>
      (limitToStock productId amount >>=
        (\left -> checkAmount left >>
          (getPrice productId >>=
            (\price -> let money = price * fromIntegral left
                       in debet user money
            )
          )
        )
      )
    )

main = do
    let user33 = User { userUid = 33, balance = 100.0 }
        user99 = User { userUid = 99, balance = 8.0 }

    putStrLn $ show $ buyMaximum user33 105 10
    putStrLn $ show $ buyMaximum user33 106 10
    putStrLn $ show $ buyMaximum user33 107 10
    putStrLn $ show $ buyMaximum user99 105  0
    putStrLn $ show $ buyMaximum user99 106  1
    putStrLn $ show $ buyMaximum user99 107  1

    putStrLn $ show $ getPrice 105
