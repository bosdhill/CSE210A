load ../../harness

@test "b8d2bb8dde2a" {
  check 'skip    ;  y    :=     x    -  x  ' '⇒ y := (x-x), {}
⇒ skip, {y → 0}'
}
