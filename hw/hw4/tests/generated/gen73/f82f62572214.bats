load ../../harness

@test "f82f62572214" {
  check 'y := x    *4 ' '⇒ skip, {y → 0}'
}
