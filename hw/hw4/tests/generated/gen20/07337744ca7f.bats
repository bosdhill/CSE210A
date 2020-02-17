load ../../harness

@test "07337744ca7f" {
  check 'if (false  ∨   true)   then 
   z     :=    x    -4     else 
 x     :=     1     +    y' '⇒ z := (x-4), {}
⇒ skip, {z → -4}'
}
