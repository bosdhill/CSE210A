load ../../harness

@test "aa631c5bd680" {
  check 'x     :=     y -   2  ' '⇒ skip, {x → -2}'
}
