load ../../harness

@test "0ac61979f805" {
  check 'if (¬(x     + qA  =    y   - y))   then  x    := -4   *    4   else  skip    ' '⇒ skip, {}'
}
