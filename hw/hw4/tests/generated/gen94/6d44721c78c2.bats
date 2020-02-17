load ../../harness

@test "6d44721c78c2" {
  check 'while false    ∨  false  do 
  skip   ' '⇒ skip, {}'
}
