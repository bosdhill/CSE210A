load ../../harness

@test "7e251e9b8a07" {
  check 'skip; VM :=   x   *   3' '⇒ VM := (x*3), {}
⇒ skip, {VM → 0}'
}
