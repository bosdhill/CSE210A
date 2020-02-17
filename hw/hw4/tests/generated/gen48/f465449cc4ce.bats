load ../../harness

@test "f465449cc4ce" {
  check 'while false   ∨   false  do 
  
z  := 0   +    z     ' '⇒ skip, {}'
}
