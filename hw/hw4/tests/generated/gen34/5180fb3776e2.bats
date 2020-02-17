load ../../harness

@test "5180fb3776e2" {
  check 'while -4  -0     *     -2=    -4  *    2   ∧     true    do    skip  ' '⇒ skip, {}'
}
