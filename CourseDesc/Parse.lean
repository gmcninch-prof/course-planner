

import CourseDesc.Course
import CourseDesc.Tokens

/- BNF
file      ::= binding*
binding   ::= ident '=' expr
expr      ::= record | list | strLit | natLit | ident | constructor
record    ::= '{' field* '}'
field     ::= ident '=' expr
list      ::= '[' (expr (',' expr)*)? ']'
constructor ::= ident record
-/

inductive Symbol where
  | LBracket | RBracket | LBrace | RBrace | Comma |Eq
deriving Repr, BEq

def symbolToken : Symbol -> Token 
  | .LBracket => Token.lbracket
  | .RBracket => Token.rbracket
  | .LBrace => Token.lbrace
  | .RBrace => Token.rbrace
  | .Comma => Token.comma
  | .Eq => Token.equals

mutual
  inductive Field where 
    | Field : String → Expression → Field
  
  inductive Expression where
    | List : List Expression -> Expression
    | StrLit : String -> Expression
    | NatLit : Nat -> Expression
    | Id : String -> Expression
    | Constructor : String -> List Field → Expression
  deriving Repr
end

def Parser (α : Type) := 
  (fuel : Nat) 
  → (toks : List Token) 
  → Except String (α × List Token)

def noFuel : String := "parser error: input too deeply nested" 

instance : Monad Parser where
  pure a := fun _ toks => Except.ok (a, toks)
  bind p f := fun fuel toks => do
    let (a, toks') ← p fuel toks
    f a fuel toks'

instance : MonadExcept String Parser where
  throw e := fun _ _ => Except.error e
  tryCatch p f := fun n toks =>
    match p n toks with
    | Except.error e => f e n toks
    | ok => ok

instance : Alternative Parser where
  failure := fun _ _ => Except.error "failure"
  orElse p q := fun fuel toks =>
    match p fuel toks with
    | Except.error _ => q () fuel toks
    | ok => ok  
  
def parseSymbol (sym:Symbol) : Parser Symbol :=
  let stok := symbolToken sym 
  fun _ toks =>
    match toks with
    | [] => Except.error "nothing to parse"
    | tok :: rest => 
      if tok == stok then
        Except.ok (sym,rest)
      else
        Except.error s!"Expected {reprStr stok} but got {reprStr tok}"

def parseString : Parser String :=
  fun _ toks =>
  match toks with
  | [] => Except.error "nothing to parse"
  | tok :: rest => match tok with
    | Token.strLit s => Except.ok (s,rest)
    | Token.ident s => Except.ok (s,rest)
    | _ => Except.error s!"wrong type: {reprStr tok} is not an string."

def parseNat : Parser Nat :=
  fun _ toks =>
  match toks with
  | [] => Except.error "nothing to parse"
  | tok :: rest => match tok with
    | Token.natLit n => Except.ok (n,rest)
    | _ => Except.error s!"wrong type: {reprStr tok} is not a Nat."


--------------------------------------------------------------------------------
mutual
  
def parseField : Parser Field := fun fuel toks =>
  match fuel with
  | 0 => .error noFuel
  | n+1 => do
    let (id, toks') ← parseString n toks
    let (_, toks'') ← parseSymbol Symbol.Eq n toks'
    let (expr, toks''') ← parseExpression n toks''
    pure (Field.Field id expr, toks''')
  
def parseFields (acc : List Field) 
    : Parser (List Field) := fun fuel toks =>
  match fuel with
  | 0 => .error noFuel
  | n+1 =>
    match toks with
    | [] => .ok (acc.reverse, [])
    | Token.rbrace :: rest => Except.ok (acc.reverse, rest)
    | Token.comma :: rest => parseFields acc n rest
    | _ => do
      let (f,toks') ← parseField n toks
      parseFields (f :: acc) n  toks' 
    
def parseExpressionList (acc : List Expression) 
    : Parser (List Expression) := fun fuel toks => 
  match fuel with 
  | 0 => .error noFuel
  | n+1 => 
    match toks with
      | [] => .ok (acc.reverse, [])
      | Token.rbracket :: rest => Except.ok (acc.reverse, rest)
      | Token.comma :: rest => parseExpressionList acc n rest
      | _ => do
        let (expr, toks') ← parseExpression n toks
        parseExpressionList (expr :: acc) n toks'

def parseExpression : Parser Expression := fun fuel toks =>
  match fuel with
  | 0 => .error noFuel
  | n+1 => 
    match toks with
    | [] => Except.error "nothing to parse"
    | tok :: rest  => match tok with
      | Token.strLit s => Except.ok (Expression.StrLit s, rest)
      | Token.natLit n => Except.ok (Expression.NatLit n, rest)
      | Token.lbracket => do
        let (exprs,toks') ← parseExpressionList [] n rest
        pure (Expression.List exprs,toks')
      | Token.ident id => 
        match rest with
        | [] => Except.ok (Expression.Id id, [])
        | tok' :: rest' => 
          match tok' with
          | Token.lbrace => do
            let (fields,toks'') ← parseFields [] n rest'
            pure (Expression.Constructor id fields,toks'')
          | _ => Except.ok (Expression.Id id, rest)
      | _ => 
        Except.error "mal-formed"


end
--------------------------------------------------------------------------------




def ex : List Token := [
  .ident "foo"
, .lbrace 
, .ident "a"
, .equals
, .natLit 1
, .ident "b"
, .equals
, .natLit 2
, .rbrace
, .comma
, .ident "GEorge"
]

#eval parseExpressions [] ex.length ex

    
