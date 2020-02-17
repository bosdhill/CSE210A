load ../../harness

@test "c246cf72c16f" {
  check 'skip ;x  :=   x' '⇒ x := x, {}
⇒ skip, {x → 0}'
}
