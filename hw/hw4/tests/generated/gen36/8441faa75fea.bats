load ../../harness

@test "8441faa75fea" {
  check 'if (x+     -4    =  x     + 1   ∧   false) then 
 skip  else      C    :=x    * -4' '⇒ C := (x*-4), {}
⇒ skip, {C → 0}'
}
