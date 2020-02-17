load ../../harness

@test "8837c917a88e" {
  check 'skip;y := -3 + -1' '⇒ y := (-3+-1), {}
⇒ skip, {y → -4}'
}
