load ../../harness

@test "34f49b81d45d" {
  check 'while false ∧true  do skip    ' '⇒ skip, {}'
}
