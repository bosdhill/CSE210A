load ../../harness

@test "fd1119fa9052" {
  check 'while 0  <   2     +-2  ∧   2  -    x    <  y     *pI    do   
 y    :=     c1    -   x; 
y    :=  TI   +     E ' '⇒ skip; y := (TI+E), {}
⇒ y := (TI+E), {}
⇒ skip, {y → 0}'
}
