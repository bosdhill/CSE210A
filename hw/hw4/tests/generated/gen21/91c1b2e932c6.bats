load ../../harness

@test "91c1b2e932c6" {
  check 'if (false  ∨   tH  *  K     =     0  +     z) then 

y     :=     x +     y    else skip     ' '⇒ y := (x+y), {}
⇒ skip, {y → 0}'
}
