inductive Token where
  | strLit (s : String)  
  | lbracket
  | rbracket
deriving Repr, BEq

inductive Expression where
    | List : List Expression -> Expression
    | StrLit : String -> Expression
deriving Repr

abbrev Parser (α : Type) := 
  (toks : List Token) → Except String (α × List Token)

mutual
  def parseExpression (fuel : Nat) : Parser Expression := fun toks =>
    match fuel with 
    | 0 => .error "out of fuel"
    | n+1 =>
      match toks with
      | .strLit s :: rest => .ok (.StrLit s, rest)
      | .lbracket :: rest  => do
          let (exprs,toks') ← parseExpressions n [] rest
          pure (.List exprs,toks')
      | _ => .error "failed to parse expr"

  def parseExpressions (fuel : Nat) (accum : List Expression) 
      : Parser (List Expression) := fun toks => 
    match fuel with
    | 0 => .error "out of fuel"
    | n+1 => match toks with
      | [] => .error "failed to parse expression list"
      | .rbracket :: rest => .ok (accum.reverse,rest)
      | _  => do
          let (expr, rest') ← parseExpression n toks
          parseExpressions n (expr::accum) rest'
    
end
    
def test : List Token := [ .lbracket
  , .strLit "foo"
  , .strLit "a"
  , .lbracket
  , .strLit "in the inner list"
  , .rbracket
  , .rbracket ]

#eval parseExpression test.length test
