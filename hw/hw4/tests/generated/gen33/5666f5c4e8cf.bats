load ../../harness

@test "5666f5c4e8cf" {
  check 'while false  do   skip     ' '⇒ skip, {}'
}
