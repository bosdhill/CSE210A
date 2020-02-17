load ../../harness

@test "a40e93c012a9" {
  check 'if (z   - n7<    H    +1 ∨   false)      then 
y  :=     FI +z     else   skip     ' '⇒ y := (FI+z), {}
⇒ skip, {y → 0}'
}
