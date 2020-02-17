load ../../harness

@test "a5a33c588b20" {
  check 'if (¬false) then 
 skip    else  
 
skip' '⇒ skip, {}'
}
