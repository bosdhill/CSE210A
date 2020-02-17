load ../../harness

@test "f8d405a3476e" {
  check 'while false    ∨ false     do  skip   ' '⇒ skip, {}'
}
