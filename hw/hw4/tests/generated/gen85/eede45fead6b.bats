load ../../harness

@test "eede45fead6b" {
  check 'skip     ; z    := z   ' '⇒ z := z, {}
⇒ skip, {z → 0}'
}
