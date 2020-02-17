load ../../harness

@test "36a389e20f91" {
  check 'if (-1     *    v = 3    +    y  ∨    true)     then   
 z  :=    y +o    else 
 
 z  :=    -1 +  0 ' '⇒ z := (y+o), {}
⇒ skip, {z → 0}'
}
