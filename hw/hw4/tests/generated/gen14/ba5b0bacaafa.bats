load ../../harness

@test "ba5b0bacaafa" {
  check 'skip     ;    z     := y   ' '⇒ z := y, {}
⇒ skip, {z → 0}'
}
