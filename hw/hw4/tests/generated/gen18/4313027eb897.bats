load ../../harness

@test "4313027eb897" {
  check 'while false  ∧   -3    *z  =     y   *   z do skip ' '⇒ skip, {}'
}
