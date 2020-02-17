load ../../harness

@test "29109d9ce021" {
  check 'while false     do    skip  ' '⇒ skip, {}'
}
