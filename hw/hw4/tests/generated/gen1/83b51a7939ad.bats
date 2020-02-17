load ../../harness

@test "83b51a7939ad" {
  check 'if (x     * c+     z  <   x -     y     ∧   true)    then 
 
 skip     else 
 
y     :=y  -   z  ' '⇒ y := (y-z), {}
⇒ skip, {y → 0}'
}
