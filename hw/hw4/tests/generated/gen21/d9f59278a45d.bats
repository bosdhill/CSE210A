load ../../harness

@test "d9f59278a45d" {
  check 'while false ∧    true      do   skip   ' '⇒ skip, {}'
}
