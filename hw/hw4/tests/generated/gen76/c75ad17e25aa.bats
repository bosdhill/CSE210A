load ../../harness

@test "c75ad17e25aa" {
  check 'while false   ∧     false  do 
  z  :=     0    +  z    ' '⇒ skip, {}'
}
