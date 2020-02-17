load ../../harness

@test "9088be0a2716" {
  check 'skip     ;y     :=  -1     +   -2  ' '⇒ y := (-1+-2), {}
⇒ skip, {y → -3}'
}
