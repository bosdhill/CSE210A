load ../../harness

@test "162b68bd9c3c" {
  check 'if (¬(1 *     y =     y     -    2))    then    skip   else   y :=     1 *    z ' '⇒ skip, {}'
}
