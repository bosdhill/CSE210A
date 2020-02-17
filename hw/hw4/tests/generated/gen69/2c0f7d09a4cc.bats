load ../../harness

@test "2c0f7d09a4cc" {
  check 'while false     do skip' '⇒ skip, {}'
}
