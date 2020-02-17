load ../../harness

@test "d114d2753bf0" {
  check 'while false   do  skip    ' '⇒ skip, {}'
}
