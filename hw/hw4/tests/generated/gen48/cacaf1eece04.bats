load ../../harness

@test "cacaf1eece04" {
  check 'skip  ; y    :=  z   +  -1  ' '⇒ y := (z+-1), {}
⇒ skip, {y → -1}'
}
