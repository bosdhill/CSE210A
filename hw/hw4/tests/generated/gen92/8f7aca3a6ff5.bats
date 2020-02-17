load ../../harness

@test "8f7aca3a6ff5" {
  check 'while false     ∧false     do skip' '⇒ skip, {}'
}
