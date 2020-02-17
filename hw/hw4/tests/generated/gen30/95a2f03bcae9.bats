load ../../harness

@test "95a2f03bcae9" {
  check 'skip;z   :=     4   *   x  ' '⇒ z := (4*x), {}
⇒ skip, {z → 0}'
}
