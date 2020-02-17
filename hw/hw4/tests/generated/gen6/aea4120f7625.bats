load ../../harness

@test "aea4120f7625" {
  check 'if (x  * z  <  2   -NC)      then 

y     :=   x else 
 y     :=y    ' '⇒ y := x, {}
⇒ skip, {y → 0}'
}
