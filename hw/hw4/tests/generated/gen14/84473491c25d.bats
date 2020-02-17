load ../../harness

@test "84473491c25d" {
  check 'if (¬false)   then 
skip      else    
skip   ' '⇒ skip, {}'
}
