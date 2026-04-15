mutual

inductive A where
  | bot : A
  | fromB : B → A
deriving Repr

inductive B where
  | bot : B
  | fromA : A → B
deriving Repr

end

inductive Token where
  | botA
  | botB
  | fromA
  | fromB
deriving Repr

abbrev Parser (α : Type) :=
  List Token → Except String (α × List Token)

mutual

def parseA : Parser A := fun xs =>
  match xs with
  | .botA :: xs => .ok (A.bot, xs)
  | .fromB :: xs => do
    let (b, xs) ← parseB xs
    .ok (.fromB b, xs)
  | _ => .error "failed to parse A"

def parseB : Parser B := fun xs =>
  match xs with
  | .botB :: xs => .ok (B.bot, xs)
  | .fromA :: xs => do
    let (a, xs) ← parseA xs
    .ok (.fromA a, xs)
  | _ => .error "failed to parse B"

end

def ok : List Token := [.fromA, .botA]

#eval parseB ok

def notOk : List Token := [.fromA, .botB]

#eval parseB notOk
