load ../../harness

@test "cc2037321f12" {
  check 'while false   ∧true do   


skip' '⇒ skip, {}'
}
