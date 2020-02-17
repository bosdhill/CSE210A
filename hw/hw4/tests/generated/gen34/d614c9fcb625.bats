load ../../harness

@test "d614c9fcb625" {
  check 'if (-2     -  S   =  u     *     4    ∧     x    <    1)   then    x:=E   *   x  else  skip   ' '⇒ skip, {}'
}
