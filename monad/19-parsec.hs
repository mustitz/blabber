module Main where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, string, digit, spaces, noneOf)
import Text.Parsec.Combinator (many1, option, sepBy)

data JSONValue = JSONNull
               | JSONBool Bool
               | JSONNumber Int
               | JSONString String
               | JSONArray [JSONValue]
               | JSONObject [(String, JSONValue)]
               deriving (Show, Eq)

jsonValue :: Parser JSONValue
jsonValue = spaces *> value <* spaces
  where
    value = choice
      [ jsonNull
      , jsonBool
      , jsonNumber
      , jsonString
      , jsonArray
      , jsonObject
      ]

jsonNull :: Parser JSONValue
jsonNull = string "null" >> return JSONNull

jsonBool :: Parser JSONValue
jsonBool = do
  value <- string "true" <|> string "false"
  return $ JSONBool (value == "true")

jsonNumber :: Parser JSONValue
jsonNumber = do
  sign <- option "" (string "-")
  digits <- many1 digit
  let number = read (sign ++ digits) :: Int
  return $ JSONNumber number

jsonString :: Parser JSONValue
jsonString = do
  char '"'
  content <- many (noneOf "\"")
  char '"'
  return $ JSONString content

jsonArray :: Parser JSONValue
jsonArray = do
  char '['
  spaces
  elements <- jsonValue `sepBy` (spaces >> char ',' >> spaces)
  spaces
  char ']'
  return $ JSONArray elements

jsonObject :: Parser JSONValue
jsonObject = do
  char '{'
  spaces
  pairs <- pair `sepBy` (spaces >> char ',' >> spaces)
  spaces
  char '}'
  return $ JSONObject pairs
  where
    pair = do
      JSONString key <- jsonString
      spaces
      char ':'
      spaces
      value <- jsonValue
      return (key, value)

parseJSON :: String -> Either ParseError JSONValue
parseJSON = parse jsonValue ""

main :: IO ()
main = do
  let jsonText = "{\"name\":\"Andrii Sevastianov\",\"age\":49,\"skills\":[\"Haskell\",\"Category theory\",\"Monads\"]}"

  case parseJSON jsonText of
    Left err -> putStrLn $ "Error: " ++ show err
    Right val -> putStrLn $ "Pasred OK: " ++ show val
