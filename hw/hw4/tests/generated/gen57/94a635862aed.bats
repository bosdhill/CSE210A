load ../../harness

@test "94a635862aed" {
  check 'while (¬(-3 +1     <y   *    y))  do    skip ' '⇒ skip, {}'
}
