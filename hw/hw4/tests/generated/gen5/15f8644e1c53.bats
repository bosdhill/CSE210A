load ../../harness

@test "15f8644e1c53" {
  check 'if (x *z = -2   - z ∧  z  +   V0  =  x)     then     x  :=     x-  -4      else   y :=    z    ' '⇒ y := z, {}
⇒ skip, {y → 0}'
}
