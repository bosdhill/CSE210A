load ../../harness

@test "03fa79026c67" {
  check 'while z     -  z  <0  * L   do skip   ' '⇒ skip, {}'
}
