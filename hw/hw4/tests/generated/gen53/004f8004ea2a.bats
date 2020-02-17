load ../../harness

@test "004f8004ea2a" {
  check 'n    := y *   0   ' '⇒ skip, {n → 0}'
}
