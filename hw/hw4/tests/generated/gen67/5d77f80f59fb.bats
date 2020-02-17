load ../../harness

@test "5d77f80f59fb" {
  check 'if (y    <   -3 - -3    ∨    x    *  Gt    < 1-     eN)  then  
y     :=     -1     *     x else  
  
skip' '⇒ y := (-1*x), {}
⇒ skip, {y → 0}'
}
