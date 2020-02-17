load ../../harness

@test "2495bfe7ff75" {
  check 'y    := z  *   4  ' '⇒ skip, {y → 0}'
}
