load ../../harness

@test "1542c3fac446" {
  check 'if (¬false)   then  
 x    :=    y  +  P    else 
 
  skip    ' '⇒ x := (y+P), {}
⇒ skip, {x → 0}'
}
