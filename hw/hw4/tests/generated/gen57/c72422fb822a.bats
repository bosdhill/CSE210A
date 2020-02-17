load ../../harness

@test "c72422fb822a" {
  check 'while (¬(-3   <     x   *    x))      do skip ' '⇒ skip, {}'
}
