load ../../harness

@test "79ebe7707da6" {
  check 'z  :=    x     -   y ' '⇒ skip, {z → 0}'
}
