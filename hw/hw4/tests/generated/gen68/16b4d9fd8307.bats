load ../../harness

@test "16b4d9fd8307" {
  check 'while false ∧ 0    + -3=     x *     z do skip   ' '⇒ skip, {}'
}
