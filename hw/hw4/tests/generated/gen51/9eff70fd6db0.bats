load ../../harness

@test "9eff70fd6db0" {
  check 'while true    ∧    false    do  z :=     -3-    y    ' '⇒ skip, {}'
}
