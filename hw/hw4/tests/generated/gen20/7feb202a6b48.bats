load ../../harness

@test "7feb202a6b48" {
  check 'y :=     y   *-1' '⇒ skip, {y → 0}'
}
