load ../../harness

@test "9907d5fd7007" {
  check 'skip     ;y     :=   -4     +   y    ' '⇒ y := (-4+y), {}
⇒ skip, {y → -4}'
}
