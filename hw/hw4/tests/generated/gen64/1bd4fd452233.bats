load ../../harness

@test "1bd4fd452233" {
  check 'while false∧  true  do      skip' '⇒ skip, {}'
}
