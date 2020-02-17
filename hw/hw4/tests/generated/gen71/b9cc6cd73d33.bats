load ../../harness

@test "b9cc6cd73d33" {
  check 'y     :=    y    - y   ' '⇒ skip, {y → 0}'
}
