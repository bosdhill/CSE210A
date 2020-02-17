load ../../harness

@test "d250c30f1b4d" {
  check 'while false  do skip ' '⇒ skip, {}'
}
