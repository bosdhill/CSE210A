load ../../harness

@test "541693f1e81c" {
  check 'y     :=     y     - 2; skip ' '⇒ skip; skip, {y → -2}
⇒ skip, {y → -2}'
}
