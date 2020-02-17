load ../../harness

@test "94cec270324e" {
  check 'while (¬(Zo    *   3    <-2 *  -3)) do skip     ' '⇒ skip, {}'
}
