load ../../harness

@test "768cab092c16" {
  check 'if (¬false)    then 
   skip      else    
z   := x   -z  ' '⇒ skip, {}'
}
