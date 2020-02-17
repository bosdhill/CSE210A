load ../../harness

@test "ad1fb187aa6e" {
  check 'x     := H    + -3     ' '⇒ skip, {x → -3}'
}
