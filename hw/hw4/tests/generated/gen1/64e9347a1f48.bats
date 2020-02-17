load ../../harness

@test "64e9347a1f48" {
  check 'skip;   z:= -3    *x   ' '⇒ z := (-3*x), {}
⇒ skip, {z → 0}'
}
