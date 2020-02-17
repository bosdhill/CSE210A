load ../../harness

@test "68f31f257abc" {
  check 'skip     ; z  := -1   *x     ' '⇒ z := (-1*x), {}
⇒ skip, {z → 0}'
}
