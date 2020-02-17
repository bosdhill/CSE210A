load ../../harness

@test "1b1eb802c6a7" {
  check 'if (true  ∧x     -  -3<    -2-    1)      then   skip  else skip ' '⇒ skip, {}'
}
