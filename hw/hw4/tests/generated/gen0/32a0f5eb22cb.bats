load ../../harness

@test "32a0f5eb22cb" {
  check 'if (true     ∧ false)     then 
y  := 2--4 else   y   :=z * z' '⇒ y := (z*z), {}
⇒ skip, {y → 0}'
}
