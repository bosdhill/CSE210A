load ../../harness

@test "a2f9b1495573" {
  check 'skip ;x  :=   0+     3  ' '⇒ x := (0+3), {}
⇒ skip, {x → 3}'
}
