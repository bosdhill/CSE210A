load ../../harness

@test "1921226c3e23" {
  check 'while (¬(-4- 0  =     x     -    4))      do skip' '⇒ skip, {}'
}
