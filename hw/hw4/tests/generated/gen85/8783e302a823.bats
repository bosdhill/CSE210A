load ../../harness

@test "8783e302a823" {
  check 'while false  ∧    false    do  skip ' '⇒ skip, {}'
}
