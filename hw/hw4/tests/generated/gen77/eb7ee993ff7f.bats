load ../../harness

@test "eb7ee993ff7f" {
  check 'x    := 2     *   x     ' '⇒ skip, {x → 0}'
}
