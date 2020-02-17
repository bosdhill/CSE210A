load ../../harness

@test "279bc1ab216a" {
  check 'skip ;y    :=    x   +  0  ' '⇒ y := (x+0), {}
⇒ skip, {y → 0}'
}
