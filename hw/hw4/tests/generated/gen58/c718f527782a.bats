load ../../harness

@test "c718f527782a" {
  check 'if (y    *3     <1    *2   ∧true)     then 

 x:=   0*x      else 
 y     :=     y -  -3' '⇒ x := (0*x), {}
⇒ skip, {x → 0}'
}
