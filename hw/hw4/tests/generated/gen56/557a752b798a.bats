load ../../harness

@test "557a752b798a" {
  check 'while false     ∧ false    do skip ' '⇒ skip, {}'
}
