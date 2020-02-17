load ../../harness

@test "e8456adf08fd" {
  check 'if (y  *   KG  =2    -   z    ∨     -3*    y = y)     then 
   x   :=    y   +-1  else  x   :=     -2  *  x    ' '⇒ x := (y+-1), {}
⇒ skip, {x → -1}'
}
