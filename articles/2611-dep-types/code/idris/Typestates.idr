import Data.So

data State = Fresh | Connected | Closed
data Op    = SetOption String | Connect | Close

Show Op where
  show (SetOption n) = "setsockopt(" ++ n ++ ")"
  show Connect       = "connect()"
  show Close         = "close()"

data Result = Accepted State | Rejected

transition : State -> Op -> Result
transition Fresh  (SetOption _) = Accepted Fresh
transition Fresh  Connect       = Accepted Connected
transition Closed Close         = Rejected
transition _      Close         = Accepted Closed
transition _      _             = Rejected

allowed : State -> Op -> Bool
allowed s op = case transition s op of
  Accepted _ => True
  Rejected   => False

step : State -> Op -> State
step s op = case transition s op of
  Accepted s' => s'
  Rejected    => s

record Socket (s : State) where
  constructor MkSocket
  history : List String

socket : Socket Fresh
socket = MkSocket ["socket()"]

exec : (op : Op) -> Socket s -> So (allowed s op) -> Socket (step s op)
exec op (MkSocket h) _ = MkSocket (h ++ [show op])

example : Socket Closed
example = do
  let s1 = exec (SetOption "SO_RCVBUF") socket Oh
  let s2 = exec (SetOption "SO_SNDBUF") s1 Oh
  let s3 = exec Connect s2 Oh
  -- let s4 = exec Connect s3 Oh   -- compiler error
  exec Close s3 Oh

main : IO ()
main = traverse_ putStrLn (history example)
