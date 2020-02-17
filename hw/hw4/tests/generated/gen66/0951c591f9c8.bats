load ../../harness

@test "0951c591f9c8" {
  check 'while false     ∨     false  do 
skip ' '⇒ skip, {}'
}
