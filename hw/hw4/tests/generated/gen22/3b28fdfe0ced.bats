load ../../harness

@test "3b28fdfe0ced" {
  check 'skip;     y    :=    -3    + -2 ' '⇒ y := (-3+-2), {}
⇒ skip, {y → -5}'
}
