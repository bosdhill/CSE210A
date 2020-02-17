load ../../harness

@test "60483958e069" {
  check 'if (4+   b  <     y     *  v1  ∨    4-  y  =     x  +   z)     then  y    :=  -1   +    4   else 

y     :=    y -  2' '⇒ y := (y-2), {}
⇒ skip, {y → -2}'
}
