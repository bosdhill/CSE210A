load ../../harness

@test "e8853771e0e8" {
  check 'if (¬false)   then 
z:=    -4    +     z     else  z :=    y    *   m ' '⇒ z := (-4+z), {}
⇒ skip, {z → -4}'
}
