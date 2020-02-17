load ../../harness

@test "50416e9afdf9" {
  check 'if (¬false)  then  
skip else 

 skip  ' '⇒ skip, {}'
}
