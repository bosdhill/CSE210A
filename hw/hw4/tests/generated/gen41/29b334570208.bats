load ../../harness

@test "29b334570208" {
  check 'if (1    *  2  - 1  *Q = x     - 2  ∧ 2+  -1  <1- -1) then 
y    := 3   *y     else z :=   -4    -     z    ' '⇒ z := (-4-z), {}
⇒ skip, {z → -4}'
}
