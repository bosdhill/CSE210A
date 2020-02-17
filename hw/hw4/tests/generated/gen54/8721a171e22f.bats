load ../../harness

@test "8721a171e22f" {
  check 'skip ;  z   :=     -1   +-1 ' '⇒ z := (-1+-1), {}
⇒ skip, {z → -2}'
}
