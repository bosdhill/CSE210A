load ../../harness

@test "7ac4549a6e10" {
  check 'x  :=-3+4;skip    ' '⇒ skip; skip, {x → 1}
⇒ skip, {x → 1}'
}
