load ../../harness

@test "a6d4cd7320a0" {
  check 'skip   ; z    := 2 ' '⇒ z := 2, {}
⇒ skip, {z → 2}'
}
