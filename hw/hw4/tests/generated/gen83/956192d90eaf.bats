load ../../harness

@test "956192d90eaf" {
  check 'while false ∧false do 
 x     :=  3     ' '⇒ skip, {}'
}
