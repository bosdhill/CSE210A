load ../../harness

@test "23e69dec5b19" {
  check 'if (1  *x  <    4  *4  ∧     false)      then 
 y :=    -1+ 0 else z    := N9   -     x   ' '⇒ z := (N9-x), {}
⇒ skip, {z → 0}'
}
