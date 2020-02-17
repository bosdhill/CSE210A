load ../../harness

@test "d689264753bd" {
  check 'while 1   -   -2 < z   *     -4   ∧     0  -1   =   x   do skip  ' '⇒ skip, {}'
}
