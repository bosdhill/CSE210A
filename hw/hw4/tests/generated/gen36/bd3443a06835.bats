load ../../harness

@test "bd3443a06835" {
  check 'if (true   ∨     -2   -   x  <   y -     4)     then  skip     else  skip     ' '⇒ skip, {}'
}
