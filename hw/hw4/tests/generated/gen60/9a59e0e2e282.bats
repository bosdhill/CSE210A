load ../../harness

@test "9a59e0e2e282" {
  check 'y :=y     +     0 ' '⇒ skip, {y → 0}'
}
