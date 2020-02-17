load ../../harness

@test "5153e0019ff3" {
  check 'while (¬(-2     *     1    <     -2  *  x))    do D :=    4 -    1   ' '⇒ skip, {}'
}
