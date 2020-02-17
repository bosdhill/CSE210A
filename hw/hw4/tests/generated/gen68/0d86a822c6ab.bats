load ../../harness

@test "0d86a822c6ab" {
  check 'while false   ∨  false do  
 y:= 2     ' '⇒ skip, {}'
}
