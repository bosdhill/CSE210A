load ../../harness

@test "b633fb608a0b" {
  check 'if (¬true)   then  z :=     -4 *    z else skip     ' '⇒ skip, {}'
}
