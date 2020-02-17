load ../../harness

@test "998795b2b423" {
  check 'y   :=     -4 *  2;skip' '⇒ skip; skip, {y → -8}
⇒ skip, {y → -8}'
}
