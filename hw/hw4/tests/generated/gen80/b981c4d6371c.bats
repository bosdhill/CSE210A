load ../../harness

@test "b981c4d6371c" {
  check 'skip ;x   :=   y    + z  ' '⇒ x := (y+z), {}
⇒ skip, {x → 0}'
}
