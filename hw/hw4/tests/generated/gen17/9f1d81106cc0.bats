load ../../harness

@test "9f1d81106cc0" {
  check 'skip    ;x    :=   y  -    -1   ' '⇒ x := (y--1), {}
⇒ skip, {x → 1}'
}
