load ../../harness

@test "9c1908cc3323" {
  check 'while false   do z  :=     y   +     x ' '⇒ skip, {}'
}
