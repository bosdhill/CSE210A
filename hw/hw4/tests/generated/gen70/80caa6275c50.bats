load ../../harness

@test "80caa6275c50" {
  check 'skip   ; z  :=-3  -E     ' '⇒ z := (-3-E), {}
⇒ skip, {z → -3}'
}
