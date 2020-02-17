load ../../harness

@test "8f7df914d498" {
  check 'while false    ∧  false do  y :=     z  ' '⇒ skip, {}'
}
