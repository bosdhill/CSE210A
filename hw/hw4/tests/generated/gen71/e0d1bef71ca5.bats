load ../../harness

@test "e0d1bef71ca5" {
  check 'skip  ;y    := -2    *   1  ' '⇒ y := (-2*1), {}
⇒ skip, {y → -2}'
}
