load ../../harness

@test "5e3e70b308e9" {
  check 'skip   ; x  :=     -4   +  -3    ' '⇒ x := (-4+-3), {}
⇒ skip, {x → -7}'
}
