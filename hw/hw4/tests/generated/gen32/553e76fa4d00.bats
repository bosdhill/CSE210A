load ../../harness

@test "553e76fa4d00" {
  check 'while false     ∧  true   do 
   skip   ' '⇒ skip, {}'
}
