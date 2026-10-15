inductive Format where
  | int : Format → Format
  | str : Format → Format
  | lit : Char → Format → Format
  | done : Format

def parse : List Char → Format
  | '%' :: 'd' :: cs => .int (parse cs)
  | '%' :: 's' :: cs => .str (parse cs)
  | c :: cs          => .lit c (parse cs)
  | []               => .done

def Printf : Format → Type
  | .int f   => Nat → Printf f
  | .str f   => String → Printf f
  | .lit _ f => Printf f
  | .done    => String

def render : (f : Format) → String → Printf f
  | .int f,   acc => fun n => render f (acc ++ toString n)
  | .str f,   acc => fun s => render f (acc ++ s)
  | .lit c f, acc => render f (acc.push c)
  | .done,    acc => acc

def cfmt (fmt : String) : Printf (parse fmt.toList) :=
  render (parse fmt.toList) ""

def putStrLn (s : String) : IO Unit := IO.println s

def main : IO Unit := do
  putStrLn $ cfmt "Spin of %s equals %d/2" "electron" 1
  putStrLn $ cfmt "Spin of %s equals %d/2" "Ω baryon" 3
  -- putStrLn $ cfmt "Spin of %s equals %d/2" 1 "proton" -- compiler error
  -- putStrLn $ cfmt "Spin of %s equals %d/2" "photon"   -- compiler error
