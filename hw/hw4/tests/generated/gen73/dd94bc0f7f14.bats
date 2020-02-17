load ../../harness

@test "dd94bc0f7f14" {
  check 'if (-3 *     i  <  3+     -2   ∧  true)     then skip   else  skip   ' '⇒ skip, {}'
}
