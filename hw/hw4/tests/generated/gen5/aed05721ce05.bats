load ../../harness

@test "aed05721ce05" {
  check 'while false   do  skip' '⇒ skip, {}'
}
