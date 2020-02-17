load ../../harness

@test "e8f12c5d7310" {
  check 'skip    ;x :=  0  -   x   ' '⇒ x := (0-x), {}
⇒ skip, {x → 0}'
}
