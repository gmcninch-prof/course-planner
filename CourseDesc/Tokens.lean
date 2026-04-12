
inductive Token
  | ident (s : String)   -- "courseAY", "EHoliday" etc
  | strLit (s : String)  -- "AY2025-2026"
  | lbrace | rbrace      -- { }
  | lbracket | rbracket  -- [ ]
  | comma | colon | equals
  | eof
deriving Repr

def isIdentChar (c : Char) : Bool := c.isAlpha || c == '_' 

def tokenize (input : String) : List Token :=
  go input.utf8ByteSize input.startPos []
where
  go (fuel : Nat) (p : String.Pos input) (acc : List Token) : List Token :=
    match fuel with 
    | 0 =>
      acc.reverse
    | n+1 =>
      if h : p = input.endPos then
        acc.reverse
      else
        let c  := p.get h
        let p' := p.next h
        match c with
        | '{' => go n p' (Token.lbrace :: acc)
        | '}' => go n p' (Token.rbrace :: acc)
        | '[' => go n p' (Token.lbracket :: acc)
        | ']' => go n p' (Token.rbracket :: acc)
        | ',' => go n p' (Token.comma :: acc)
        | ':' => go n p' (Token.colon :: acc)
        | '=' => go n p' (Token.equals :: acc)
        | '-' =>
          if h' : p' = input.endPos then
            go n p' acc  -- single dash at end of input, skip
          else
            if p'.get h' == '-' then
              let p'' := skipToNewline n p' 
              go n p'' acc
            else
              go n p' acc  -- single dash, skip or error        
        | '\x22'  => -- double quote
            let (s, p'') := scanString n p' ""
            go n p'' (Token.strLit s :: acc)
        | c   =>
          if isIdentChar c then
            let (s,p'') := scanIdent n p' (String.singleton c)
            go n p'' (Token.ident s::acc)
          else if c.isWhitespace then
            go n p' acc  -- skip
          else
            go n p' acc  -- unknown, skip or error
  termination_by fuel


  scanString (fuel : Nat) (p : input.Pos) (acc : String) : (String × input.Pos) :=
    match fuel with
    | 0 => (acc,p)
    | n+1 => 
      if h : p = input.endPos then
        (acc, p)  -- unterminated string literal, return what we have
      else
        let c := p.get h
        let p' := p.next h
        if c == '\x22' then
          (acc, p')  -- closing quote, done
        else
          scanString n p' (acc.push c  )


  scanIdent (fuel : Nat) (p : input.Pos) (acc : String) : (String × input.Pos) :=
    match fuel with
    | 0 => (acc,p)
    | n+1 => 
      if h : p = input.endPos then
        (acc, p)  
      else
        let c := p.get h
        let p' := p.next h
        if isIdentChar c then
          scanIdent n p' (acc.push c)        
        else
          (acc, p)  -- return p, not p' since we don't consume c

  skipToNewline (fuel : Nat) (p : input.Pos) : input.Pos :=
    match fuel with
    | 0 => p
    | n + 1 =>
      if h : p = input.endPos then
        p
      else
        let c := p.get h
        let p' := p.next h
        if c == '\n' then
          p'
        else
          skipToNewline n p'
    
#eval tokenize "{foo = \"hello\", bar = [1,2], \"baz\"} -- foo bar baz\nack = \"baz\""
