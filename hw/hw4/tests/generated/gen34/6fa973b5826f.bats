load ../../harness

@test "6fa973b5826f" {
  check 'while y     <  -3 ∧ false do skip ' '⇒ skip, {}'
}
