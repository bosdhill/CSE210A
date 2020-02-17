load ../../harness

@test "c62ba431040b" {
  check 'if (true ∨     false)   then   


skip     else  
skip' '⇒ skip, {}'
}
