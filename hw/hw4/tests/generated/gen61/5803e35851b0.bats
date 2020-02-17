load ../../harness

@test "5803e35851b0" {
  check 'skip    ; z   :=   x     -2     ' '⇒ z := (x-2), {}
⇒ skip, {z → -2}'
}
