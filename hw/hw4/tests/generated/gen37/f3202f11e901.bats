load ../../harness

@test "f3202f11e901" {
  check 'while false     ∧    false     do x  := m1     + t     ' '⇒ skip, {}'
}
