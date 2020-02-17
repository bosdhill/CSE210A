load ../../harness

@test "1c321d2f6718" {
  check 'y  :=     1 -   x' '⇒ skip, {y → 1}'
}
