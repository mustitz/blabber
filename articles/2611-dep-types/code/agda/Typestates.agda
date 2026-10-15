{-# OPTIONS --guardedness #-}

open import Data.Bool   using (Bool ; true ; false)
open import Data.String using (String ; _++_ ; unlines)
open import Data.List   using (List ; [_] ; _∷ʳ_)
open import Function    using (_$_)
open import IO          using (Main ; run ; putStrLn)

data State : Set where
  fresh connected closed : State

data Op : Set where
  setOption : String → Op
  connect close : Op

show : Op → String
show (setOption n) = "setsockopt(" ++ n ++ ")"
show connect       = "connect()"
show close         = "close()"

data Result : Set where
  accepted : State → Result
  rejected : Result

transition : State → Op → Result
transition fresh  (setOption _) = accepted fresh
transition fresh  connect       = accepted connected
transition closed close         = rejected
transition _      close         = accepted closed
transition _      _             = rejected

allowed : State → Op → Bool
allowed s op with transition s op
... | accepted _ = true
... | rejected   = false

step : State → Op → State
step s op with transition s op
... | accepted s′ = s′
... | rejected    = s

record Socket (s : State) : Set where
  constructor mkSocket
  field
    history : List String

socket : Socket fresh
socket = mkSocket [ "socket()" ]

data So : Bool → Set where
  oh : So true

exec : {s : State} (op : Op) → Socket s → So (allowed s op) → Socket (step s op)
exec op (mkSocket h) _ = mkSocket (h ∷ʳ show op)

example : Socket closed
example = do
  let s₁ = exec (setOption "SO_RCVBUF") socket oh
  let s₂ = exec (setOption "SO_SNDBUF") s₁ oh
  let s₃ = exec connect s₂ oh
  -- let s₄ = exec connect s₃ oh   -- compiler error
  exec close s₃ oh

main : Main
main = run $ putStrLn $ unlines $ Socket.history example
