load ../../harness

@test "bffdee84a42b" {
  check 'if (¬false)   then  
skip   else    skip   ' '⇒ skip, {}'
}
