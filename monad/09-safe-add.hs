safeAdd :: Maybe Int -> Maybe Int -> Maybe Int
safeAdd a b = (+) <$> a <*> b

pp :: Maybe Int -> String
pp Nothing = "null"
pp (Just x) = show x

test :: Maybe Int -> Maybe Int -> String
test a b =
    let c = safeAdd a b in
    (pp a) ++ " + " ++ (pp b) ++ " = " ++ (pp c)

main = do
    putStrLn $ test (Just 5) (Just 7)
    putStrLn $ test Nothing (Just 7)
    putStrLn $ test (Just 5) Nothing
    putStrLn $ test Nothing Nothing
