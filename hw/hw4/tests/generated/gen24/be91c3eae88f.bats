load ../../harness

@test "be91c3eae88f" {
  check 'y    := 4   +    4' '⇒ skip, {y → 8}'
}
