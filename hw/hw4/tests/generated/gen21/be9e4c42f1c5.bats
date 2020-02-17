load ../../harness

@test "be9e4c42f1c5" {
  check 'if (true     ∨     2   -  z    <  0     +    z)   then y     :=z  *    1  else   skip' '⇒ y := (z*1), {}
⇒ skip, {y → 0}'
}
