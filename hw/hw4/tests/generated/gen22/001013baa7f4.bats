load ../../harness

@test "001013baa7f4" {
  check 'if (¬(z +     -4<U     +     y))   then z  :=-2    *    -3  else  skip     ' '⇒ skip, {}'
}
