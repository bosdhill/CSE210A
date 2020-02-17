load ../../harness

@test "1fa358bbfe14" {
  check 'while x     =     -1+y  ∨z  +  z    <     y    *   z   do    z    :=  -4 *   3 ' '⇒ skip, {}'
}
