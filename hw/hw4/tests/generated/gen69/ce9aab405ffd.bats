load ../../harness

@test "ce9aab405ffd" {
  check 'x     :=    1 *   -2   ' '⇒ skip, {x → -2}'
}
