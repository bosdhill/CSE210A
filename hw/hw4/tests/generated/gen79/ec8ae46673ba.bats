load ../../harness

@test "ec8ae46673ba" {
  check 'y :=  y - -4   ' '⇒ skip, {y → 4}'
}
