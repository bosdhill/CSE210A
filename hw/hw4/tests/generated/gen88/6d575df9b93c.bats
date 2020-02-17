load ../../harness

@test "6d575df9b93c" {
  check 'while false  ∧ true      do   S  := -1 ' '⇒ skip, {}'
}
