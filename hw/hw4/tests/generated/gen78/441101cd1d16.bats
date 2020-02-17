load ../../harness

@test "441101cd1d16" {
  check 'while (¬(0  -   1 =    x     +   -1))      do   skip   ' '⇒ skip, {}'
}
