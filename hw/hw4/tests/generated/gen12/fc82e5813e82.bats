load ../../harness

@test "fc82e5813e82" {
  check 'y:=    -3   + Yn;skip  ' '⇒ skip; skip, {y → -3}
⇒ skip, {y → -3}'
}
