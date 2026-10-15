inductive Vec (α : Type) : Nat → Type where
  | nil  : Vec α 0
  | cons : α → Vec α n → Vec α (n + 1)

def nth : (v : Vec String n) → (m : Nat) → Nat.blt m n → String
  | .cons x _,  0,     _ => x
  | .cons _ xs, k + 1, h => nth xs k h

def sample : Vec String 5 :=
  .cons "a" (.cons "b" (.cons "c" (.cons "d" (.cons "e" .nil))))

def third : String := nth sample 2 rfl

-- Compiler error
-- def seventh : String := nth sample 6 rfl

def nth? (v : Vec String n) (m : Nat) : Option String :=
  if h : Nat.blt m n
    then some (nth v m h)
    else none

def main : IO Unit := do
  IO.println third
  -- IO.println seventh
  IO.println $ toString $ nth? sample 2
  IO.println $ toString $ nth? sample 7
