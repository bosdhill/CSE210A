load ../../harness

@test "715e1b360047" {
  check 'y   :=    0  +    -1  ' '⇒ skip, {y → -1}'
}
