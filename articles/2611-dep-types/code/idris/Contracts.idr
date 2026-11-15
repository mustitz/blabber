import Data.Vect
import Data.So

nth : (v : Vect n String) -> (m : Nat) -> So (m < n) -> String
nth []        Z     p = absurd p
nth []        (S _) p = absurd p
nth (x :: _)  Z     _ = x
nth (_ :: xs) (S k) p = nth xs k p

example : Vect 5 String
example = "a" :: "b" :: "c" :: "d" :: "e" :: []

third : String
third = nth example 2 Oh

-- Compiler error
-- seventh : String
-- seventh = nth example 6 Oh

nth' : {n : Nat} -> (xs : Vect n String) -> (m : Nat) -> Maybe String
nth' {n} xs m = case choose (m < n) of
  Left  p => Just (nth xs m p)
  Right _ => Nothing

main : IO ()
main = do
  putStrLn third
  -- putStrLn seventh
  putStrLn $ show $ nth' example 2
  putStrLn $ show $ nth' example 7
