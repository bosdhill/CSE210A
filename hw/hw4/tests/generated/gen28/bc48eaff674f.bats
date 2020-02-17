load ../../harness

@test "bc48eaff674f" {
  check 'if (false  ∨     y  -4=-3   +    -3)     then 

 
skip   else E    :=    -2   +  -4 ' '⇒ E := (-2+-4), {}
⇒ skip, {E → -6}'
}
