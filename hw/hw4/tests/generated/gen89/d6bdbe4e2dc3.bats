load ../../harness

@test "d6bdbe4e2dc3" {
  check 'y    := y     +    -1    ' '⇒ skip, {y → -1}'
}
