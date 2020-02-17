load ../../harness

@test "133633be02da" {
  check 'while (¬(A4 *    z   =     3     *     x))     do  skip  ' '⇒ skip, {}'
}
