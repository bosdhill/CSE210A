load ../../harness

@test "a8494c41bd70" {
  check 'if (1     -    z   <     1   -     0∨   x     * -4    < -3     -    2)      then   skip  else   skip  ' '⇒ skip, {}'
}
