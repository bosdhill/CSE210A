load ../../harness

@test "3af3ddafdb27" {
  check 'while (¬(2  *  x =    0 -y))  do     y:=-1   * -3  ' '⇒ skip, {}'
}
