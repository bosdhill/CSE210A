load ../../harness

@test "2ded3028f163" {
  check 'if (¬(y   *x    < -2))      then  skip else    z   :=     -2  * -2  ' '⇒ skip, {}'
}
