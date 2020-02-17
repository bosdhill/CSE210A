load ../../harness

@test "c77351d75ad1" {
  check 'y    :=     y *   0  ' '⇒ skip, {y → 0}'
}
