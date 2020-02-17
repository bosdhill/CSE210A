load ../../harness

@test "8e11f77886a6" {
  check 'skip    ;x :=     -4    -  x   ' '⇒ x := (-4-x), {}
⇒ skip, {x → -4}'
}
