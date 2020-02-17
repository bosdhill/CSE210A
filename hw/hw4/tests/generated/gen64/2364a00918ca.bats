load ../../harness

@test "2364a00918ca" {
  check 'while false     ∧     true   do 
skip  ' '⇒ skip, {}'
}
