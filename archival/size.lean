--------------------------------------------------------------------------------
mutual
  inductive Field where 
    | Field : String → Expression → Field
  
  inductive Expression where
    | EList : List Expression -> Expression
    | StrLit : String -> Expression
    | NatLit : Nat -> Expression
    | Id : String -> Expression
    | Record : String -> List Field → Expression
  deriving Repr

end
--------------------------------------------------------------------------------

mutual
  def Field.size : Field → Nat
    | .Field _ expr => expr.size + 1
  def Expression.size : Expression → Nat
    | .EList xs => xs.foldl (fun acc x => acc + x.size) 0 + 1
    | .Record _ fields => fields.foldl (fun acc f => acc + f.size) 0 + 1
    | _ => 1
end

theorem foldl_acc_le (xs : List Expression) (acc : Nat) : 
    acc ≤ xs.foldl (fun a x => a + x.size) acc := by
  induction xs generalizing acc with
  | nil => simp
  | cons hd tl ih => 
    simp [List.foldl]
    have := ih (acc + hd.size)
    omega
    
theorem foldl_mono (xs : List Expression) (m n : Nat) (h : m ≤ n) :
    xs.foldl (fun acc x => acc + x.size) m ≤ 
    xs.foldl (fun acc x => acc + x.size) n := by
  induction xs generalizing m n with
  | nil => simp; exact h
  | cons hd tl ih =>
    simp [List.foldl]
    apply ih
    omega    
    
theorem Expression.size_lt_list (a : Expression) (xs : List Expression) (h : a ∈ xs) :
    a.size < (Expression.EList xs).size := by 
  induction xs with
  | nil => exact absurd h (by simp)
  | cons hd tl ih => 
      simp [List.mem_cons] at h
      cases h with
      | inl h =>
        subst h
        simp [Expression.size]
        have := foldl_acc_le tl a.size
        omega      
      | inr h => 
        have := ih h
        simp [Expression.size] at *
        have k := foldl_acc_le tl a.size
        have h2 := foldl_mono tl 0 hd.size (by omega)
        omega        

theorem Expression.size_lt_constructor (a : Field) (id : String) (lst : List Field) (h : a ∈ lst) :
    a.size < (Expression.Record id lst).size := by sorry


