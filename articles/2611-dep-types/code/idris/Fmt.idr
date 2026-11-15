data Format = Number Format | Str Format | Lit Char Format | End

parse : List Char -> Format
parse ('%' :: 'd' :: cs) = Number (parse cs)
parse ('%' :: 's' :: cs) = Str (parse cs)
parse (c :: cs)          = Lit c (parse cs)
parse []                 = End

Printf : Format -> Type
Printf (Number f) = Nat -> Printf f
Printf (Str f)    = String -> Printf f
Printf (Lit _ f)  = Printf f
Printf End        = String

render : (f : Format) -> String -> Printf f
render (Number f) acc = \n => render f (acc ++ show n)
render (Str f)    acc = \s => render f (acc ++ s)
render (Lit c f)  acc = render f (acc ++ pack [c])
render End        acc = acc

cfmt : (fmt : String) -> Printf (parse (unpack fmt))
cfmt fmt = render (parse (unpack fmt)) ""

main : IO ()
main = do
  putStrLn $ cfmt "Spin of %s equals %d/2" "electron" 1
  putStrLn $ cfmt "Spin of %s equals %d/2" "Ω baryon" 3
  -- putStrLn $ cfmt "Spin of %s equals %d/2" 1 "proton" -- compiler error
  -- putStrLn $ cfmt "Spin of %s equals %d/2" "photon"   -- compiler error
