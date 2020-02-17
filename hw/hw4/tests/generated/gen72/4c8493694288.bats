load ../../harness

@test "4c8493694288" {
  check 'if (false    ∧ false) then   skip else  z :=z -   z ' '⇒ z := (z-z), {}
⇒ skip, {z → 0}'
}
