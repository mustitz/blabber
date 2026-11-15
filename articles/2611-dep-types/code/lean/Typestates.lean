inductive State where
  | fresh
  | connected
  | closed

inductive Op where
  | setOption (name : String)
  | connect
  | close

instance : ToString Op where
  toString
    | .setOption n => s!"setsockopt({n})"
    | .connect => "connect()"
    | .close => "close()"

inductive Result where
  | accepted (s : State)
  | rejected

def transition : State → Op → Result
  | .fresh,  .setOption _ => .accepted .fresh
  | .fresh,  .connect     => .accepted .connected
  | .closed, .close       => .rejected
  | _,       .close       => .accepted .closed
  | _,       _            => .rejected

def allowed (s : State) (op : Op) : Bool :=
  match transition s op with
  | .accepted _ => true
  | .rejected   => false

def step (s : State) (op : Op) : State :=
  match transition s op with
  | .accepted s' => s'
  | .rejected    => s

structure Socket (s : State) where
  mkSocket :: (history : List String)

def socket : Socket .fresh := .mkSocket ["socket()"]

def exec (op : Op) : Socket s → allowed s op → Socket (step s op)
  | .mkSocket h, _ => .mkSocket (h ++ [toString op])

def sample : Socket .closed :=
  let s1 := exec (.setOption "SO_RCVBUF") socket rfl
  let s2 := exec (.setOption "SO_SNDBUF") s1 rfl
  let s3 := exec .connect s2 rfl
  -- let s4 := exec .connect s3 rfl   -- compiler error
  exec .close s3 rfl

def main : IO Unit := do
  for line in sample.history do
    IO.println line
