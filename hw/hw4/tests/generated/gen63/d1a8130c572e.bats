load ../../harness

@test "d1a8130c572e" {
  check 'while false     do   skip    ' '⇒ skip, {}'
}
