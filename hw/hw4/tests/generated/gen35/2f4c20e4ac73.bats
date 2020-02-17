load ../../harness

@test "2f4c20e4ac73" {
  check 'y :=     y; skip ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
