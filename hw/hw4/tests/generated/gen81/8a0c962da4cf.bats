load ../../harness

@test "8a0c962da4cf" {
  check 'x    := nS    -x  ;z     := 2 ' '⇒ skip; z := 2, {x → 0}
⇒ z := 2, {x → 0}
⇒ skip, {x → 0, z → 2}'
}
