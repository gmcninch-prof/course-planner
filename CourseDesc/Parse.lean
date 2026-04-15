

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
end

def Parser (α : Type) := (toks:List Token) 
  → Except String (α × { rest:List Token // rest.length < toks.length })

instance : Monad Parser where
  pure a := fun toks => Except.ok (a, toks)
  bind p f := fun toks => do
    let (a, toks') ← p toks
    f a toks'

instance : MonadExcept String Parser where
  throw e := fun _ => Except.error e
  tryCatch p f := fun toks =>
    match p toks with
    | Except.error e => f e toks
    | ok => ok

-- class Alternative (f : Type → Type) extends Applicative f where
--   failure : f α
--   orElse : f α → (Unit → f α) → f α

instance : Alternative Parser where
  failure := fun _ => Except.error "failure"
  orElse p q := fun toks =>
    match p toks with
    | Except.error _ => (q ()) toks
    | ok => ok

mutual

  def parseSymbol (sym:Symbol) : Parser Symbol :=
    let stok := symbolToken sym 
    fun toks =>
      match toks with
      | [] => Except.error "nothing to parse"
      | tok :: rest => 
        if tok == stok then
          Except.ok (sym,rest)
        else
          Except.error s!"Expected {reprStr stok} but got {reprStr tok}"

  def parseString : Parser String :=
    fun toks =>
    match toks with
    | [] => Except.error "nothing to parse"
    | tok :: rest => match tok with
      | Token.strLit s => Except.ok (s,rest)
      | Token.ident s => Except.ok (s,rest)
      | _ => Except.error s!"wrong type: {reprStr tok} is not an string."

  def parseNat : Parser Nat :=
    fun toks =>
    match toks with
    | [] => Except.error "nothing to parse"
    | tok :: rest => match tok with
      | Token.natLit n => Except.ok (n,rest)
      | _ => Except.error s!"wrong type: {reprStr tok} is not a Nat."
  
  def parseField : Parser Field := do
    let id ← parseString
    let _ ← parseSymbol Symbol.Eq
    let expr ← parseExpression
    pure (Field.Field id expr)
  
  def parseFields : Parser (List Field) :=
    fun toks => pfields toks []
  where
    pfields (toks :List Token) (acc : List Field) 
      : Except String (List Field × List Token) :=
    match toks with
    | Token.rbrace :: rest => Except.ok (acc.reverse, rest)
    | Token.comma :: rest => pfields rest acc
    | _ => do
      let (expr,toks') ← parseField toks
      pfields toks' (expr :: acc)
    
  def parseExpressions : Parser (List Expression) :=
    fun toks => pexprs toks []
  where
    pexprs (toks : List Token) (acc : List Expression) 
        : Except String (List Expression × List Token) :=
      match toks with
      | Token.rbracket :: rest => Except.ok (acc.reverse, rest)
      | Token.comma :: rest => pexprs rest acc
      | _ => do
        let (expr, toks') ← parseExpression toks
        pexprs toks' (expr :: acc)  

  def parseExpression : Parser Expression 
  | [] => Except.error "nothing to parse"
  | tok :: rest  => match tok with
      | Token.strLit s => Except.ok (Expression.StrLit s, rest)
      | Token.natLit n => Except.ok (Expression.NatLit n, rest)
      | Token.lbracket => do
        let (exprs,toks') ← parseExpressions rest
        pure (Expression.List exprs,toks')
      | Token.ident id => 
        match rest with
        | [] => Except.ok (Expression.Id id, [])
        | tok' :: rest' => 
          match tok' with
          | Token.lbrace => do
            let (fields,toks'') ← parseFields rest'
            pure (Expression.Constructor id fields,toks'')
          | _ => Except.ok (Expression.Id id, rest)
      | _ => 
        Except.error "mal-formed"


end





