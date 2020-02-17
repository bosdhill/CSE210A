load ../../harness

@test "b3caff060ffb" {
  check 'if (z   *     P     <     4     *     x∧   z    -  -1     < x)   then skip else skip  ' '⇒ skip, {}'
}
