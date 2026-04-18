
--------------------------------------------------------------------------------
mutual
  inductive Field where 
    | mk : String → Expression → Field
  
  inductive Expression where
    | EList : List Expression -> Expression
    | StrLit : String -> Expression
    | NatLit : Nat -> Expression
    | BoolLit : Bool -> Expression
    | Id : String -> Expression
    | Constructor : String -> List Field → Expression
  deriving Repr

end
--------------------------------------------------------------------------------

def Field.name : Field → String
| .mk name _ => name

--------------------------------------------------------------------------------

mutual
  def Field.size : Field → Nat
    | .mk _ expr => expr.size + 1
  def Expression.size : Expression → Nat
    | .EList xs => expressionListSize xs + 1
    | .Constructor _ fields => fieldListSize fields + 1
    | _ => 1
  private def expressionListSize : List Expression → Nat
    | [] => 0
    | x :: xs => x.size + expressionListSize xs
  private def fieldListSize : List Field → Nat
    | [] => 0
    | x :: xs => x.size + fieldListSize xs
end

theorem Expression.size_lt_list (a : Expression) (xs : List Expression) (h : a ∈ xs) :
    a.size < (Expression.EList xs).size := by 
  induction xs with
  | nil => exact absurd h (by simp)
  | cons hd tl ih => 
    simp [List.mem_cons] at h
    simp [Expression.size]
    cases h with
    | inl h => subst h; simp [expressionListSize]; omega
    | inr h => 
      have := ih h 
      simp [expressionListSize, Expression.size] at *
      omega 

theorem Expression.size_lt_constructor (a : Field) (id : String) (fs : List Field) (h : a ∈ fs) :
    a.size < (Expression.Constructor id fs).size := by 
  induction fs with
  | nil => exact absurd h (by simp)
  | cons hd tl ih => 
    simp [List.mem_cons] at h
    simp [Expression.size]
    cases h with
    | inl h => subst h; simp [fieldListSize]; omega
    | inr h => 
      have := ih h
      simp [fieldListSize, Expression.size] at *
      omega

mutual 
  def Field.render : Field -> String := fun field =>
    match field with
    | .mk id val => id ++ " = " ++ val.render 
  termination_by f => f.size
  decreasing_by
    simp [Field.size]

  def Expression.render : Expression -> String := fun expr =>
    match expr with
    | .EList xs => 
           String.intercalate ", " (xs.attach.map (fun ⟨a, _⟩ => Expression.render a))    
    | .StrLit s => s
    | .NatLit n => toString n
    | .BoolLit b => toString b
    | .Id id => id
    | .Constructor id lst => 
        let fstring := String.intercalate ", " (lst.attach.map (fun ⟨a, _⟩ => Field.render a))
        s!"{id} [ {fstring} ] "
  termination_by e => e.size              
  decreasing_by
    · rename_i h
      apply Expression.size_lt_list _ _ h
    · rename_i h
      apply Expression.size_lt_constructor _ _ _ h
end


instance : ToString Expression where
  toString := Expression.render

instance : ToString Field where
  toString := Field.render


def lookupField (name : String) (fields : List Field) : Except String Expression :=
  match fields.find? (fun f => f.name == name) with
  | some (.mk _ expr) => .ok expr
  | none => .error s!"missing field: {name}"

