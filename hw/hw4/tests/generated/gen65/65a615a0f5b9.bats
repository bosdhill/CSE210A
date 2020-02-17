load ../../harness

@test "65a615a0f5b9" {
  check 'while (¬(-2   + -2     < 1    *    z))      do    skip' '⇒ skip, {}'
}
