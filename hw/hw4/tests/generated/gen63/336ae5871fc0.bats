load ../../harness

@test "336ae5871fc0" {
  check 'while ¬(FT  +WG = y) ∧ true  do  skip' '⇒ skip, {}'
}
