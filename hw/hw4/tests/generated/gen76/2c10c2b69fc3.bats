load ../../harness

@test "2c10c2b69fc3" {
  check 'while false ∧  true do 
  skip    ' '⇒ skip, {}'
}
