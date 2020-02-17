load ../../harness

@test "3f78ed602c0c" {
  check 'skip     ;z :=  -2 *x   ' '⇒ z := (-2*x), {}
⇒ skip, {z → 0}'
}
