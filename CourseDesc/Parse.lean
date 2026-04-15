

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
  
  inductive Record  where
    | Rec : List Field → Record
  
  inductive Expression where
    | ERecord : Record  → Expression
    | EList : (exps : List Expression) -> Expression
    | EStrLit : (a:String) -> Expression
    | ENatLit : (n:Nat) -> Expression
    | EId : (id:String) -> Expression
    | EConstructor : (id:String) -> (rec:Record) → Expression
end

def Parser (α : Type) := List Token → Except String (α × List Token)

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
--   orElse : f α → f α → f α

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
  
  def parseRecord : Parser Record := sorry

  def parseList : Parser (List Expression) :=
    fun toks => parseListItems toks []
  where
    parseListItems (toks : List Token) (acc : List Expression) 
        : Except String (List Expression × List Token) :=
      match toks with
      | Token.rbracket :: rest => Except.ok (acc.reverse, rest)
      | Token.comma :: rest => parseListItems rest acc
      | _ => do
        let (expr, toks') ← parseExpression toks
        parseListItems toks' (expr :: acc)
    termination_by toks.length
  

  def parseExpression : Parser Expression 
  | [] => Except.error "nothing to parse"
  | tok :: rest => match tok with
    | Token.lbrace => do
      let (rec,toks') ← parseRecord rest 
      pure (Expression.ERecord rec,toks')
    | Token.lbracket => do
      let (ll,toks') ← parseList rest
      pure (Expression.EList ll, toks')


end





