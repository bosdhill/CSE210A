load ../../harness

@test "d2f8a5220b4f" {
  check 'skip   ;y    := 3   -     x  ' '⇒ y := (3-x), {}
⇒ skip, {y → 3}'
}
