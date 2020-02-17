load ../../harness

@test "9766c275d09e" {
  check 'y    :=4   -   -4 ' '⇒ skip, {y → 8}'
}
