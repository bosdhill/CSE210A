load ../../harness

@test "8309d93bca02" {
  check 'y     :=   1-     x' '⇒ skip, {y → 1}'
}
