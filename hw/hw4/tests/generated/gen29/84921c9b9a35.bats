load ../../harness

@test "84921c9b9a35" {
  check 'if (-2   *     y   < x   -4  ∧ z   +  3  < x  +  x)     then 
 
x     :=     3    +  zP      else 
  z:= -3+  4' '⇒ z := (-3+4), {}
⇒ skip, {z → 1}'
}
