load ../../harness

@test "787369047815" {
  check 'while false     ∧  false     do  
skip    ' '⇒ skip, {}'
}
