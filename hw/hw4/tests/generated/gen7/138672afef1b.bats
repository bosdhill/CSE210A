load ../../harness

@test "138672afef1b" {
  check 'if (3   *    x*    2     =     -4     * y  ∨     true)   then skip  else   skip ' '⇒ skip, {}'
}
