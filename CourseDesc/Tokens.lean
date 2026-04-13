
inductive Token
  | ident (s : String)   -- "courseAY", "EHoliday" etc
  | strLit (s : String)  -- "AY2025-2026"
  | natLit (n : Nat)
  | lbrace | rbrace      -- { }
  | lbracket | rbracket  -- [ ]
  | comma | colon | equals
  | eof
deriving Repr

def isIdentChar (c : Char) : Bool := c.isAlpha || c == '_' 

def tokenize (input : String) : List Token :=
  go input.startPos []
where
  go (p : String.Pos input) (acc : List Token) : List Token :=
      if h : p = input.endPos then
        acc.reverse
      else
        let c  := p.get h
        let p' := p.next h
        match c with
        | '{' => go p' (Token.lbrace :: acc)
        | '}' => go p' (Token.rbrace :: acc)
        | '[' => go p' (Token.lbracket :: acc)
        | ']' => go p' (Token.rbracket :: acc)
        | ',' => go p' (Token.comma :: acc)
        | ':' => go p' (Token.colon :: acc)
        | '=' => go p' (Token.equals :: acc)
        | '-' =>
          if h' : p' = input.endPos then
            go p' acc  -- single dash at end of input, skip
          else
            if p'.get h' == '-' then
              let ⟨p'',h⟩ := skipToNewline p' 
              have k : p < p'' := by 
                simp                
              go p'' acc
            else
              go p' acc  -- single dash, skip or error        
        | '\x22'  => -- double quote
            let (s, p'') := scanString p' ""
            go p'' (Token.strLit s :: acc)
        | c   =>
          if isIdentChar c then
            let (s,p'') := scanIdent p' (String.singleton c)
            go p'' (Token.ident s::acc)
          else if c.isDigit then
            let (s,p'') := scanDigits p' (String.singleton c)
            let r : Nat := s.toNat! -- or: Option.elim s.toNat? 0 id
            go p'' (Token.natLit r :: acc)
          else if c.isWhitespace then
            go p' acc  -- skip
          else
            go p' acc  -- unknown, skip or error
  termination_by p


  scanString (p : input.Pos) (acc : String) : (String × input.Pos) :=
      if h : p = input.endPos then
        (acc, p)  -- unterminated string literal, return what we have
      else
        let c := p.get h
        let p' := p.next h
        if c == '\x22' then
          (acc, p')  -- closing quote, done
        else
          scanString p' (acc.push c  )


  scanIdent (p : input.Pos) (acc : String) : (String × input.Pos) :=
      if h : p = input.endPos then
        (acc, p)  
      else
        let c := p.get h
        let p' := p.next h
        if isIdentChar c then
          scanIdent p' (acc.push c)        
        else
          (acc, p)  -- return p, not p' since we don't consume c

  scanDigits (p : input.Pos) (acc : String) : (String × { q : input.Pos // p ≤ q } ) :=
    if h : p = input.endPos then
      (acc, ⟨p , by simp⟩)  
    else
      let c := p.get h
      let p' := p.next h
      have : p ≤ p' := by exact String.Pos.le_next
      if c.isDigit then
        let (s,⟨ p'', h'' ⟩) := scanDigits p' (acc.push c)        
        (s, ⟨ p'', by exact String.Pos.le_trans this h'' ⟩)
      else
        (acc, ⟨p,by simp⟩)  -- return p, not p' since we don't consume c
  termination_by p

  skipToNewline (p : input.Pos) : { q : input.Pos // p ≤ q } :=
    if h : p = input.endPos then
      ⟨p, by simp ⟩
    else
      let c := p.get h
      let p' := p.next h
      have : p ≤ p' := by exact String.Pos.le_next
      if c == '\n' then
        ⟨p', this ⟩
      else
        let ⟨ p'', h'' ⟩ := skipToNewline p'
        ⟨ p'', by exact String.Pos.le_trans this h'' ⟩
  termination_by p
    
#eval tokenize "{foo = \"hello\", bar = [1,2], \"baz\"} -- foo bar baz\nack = \"baz\""


def countSpaces (input : String) : Nat :=
  go input.startPos 0
where
  go (p : input.Pos) (acc : Nat) : Nat :=
    if h : p = input.endPos then
      acc
    else
      let c := p.get h
      let p' := p.next h
      go p' (if c == ' ' then acc + 1 else acc)
  termination_by p


#eval countSpaces "asdf  asdf"
