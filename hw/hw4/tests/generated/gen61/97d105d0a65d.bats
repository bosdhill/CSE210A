load ../../harness

@test "97d105d0a65d" {
  check 'while false   do skip    ' '⇒ skip, {}'
}
