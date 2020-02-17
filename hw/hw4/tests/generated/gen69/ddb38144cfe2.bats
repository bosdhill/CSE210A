load ../../harness

@test "ddb38144cfe2" {
  check 'if (true   ∧  false)   then 
  
skip   else     C  :=2*     3   ' '⇒ C := (2*3), {}
⇒ skip, {C → 6}'
}
