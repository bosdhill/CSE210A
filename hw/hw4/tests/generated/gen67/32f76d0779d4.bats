load ../../harness

@test "32f76d0779d4" {
  check 'skip   ; y  :=  z   +    -4     ' '⇒ y := (z+-4), {}
⇒ skip, {y → -4}'
}
