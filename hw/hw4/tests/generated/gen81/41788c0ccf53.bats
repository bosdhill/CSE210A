load ../../harness

@test "41788c0ccf53" {
  check 'while (¬(x     *   -2     = zc))   do skip ' '⇒ skip, {}'
}
