load ../../harness

@test "7b8504163294" {
  check 'y :=x    *   x     ' '⇒ skip, {y → 0}'
}
