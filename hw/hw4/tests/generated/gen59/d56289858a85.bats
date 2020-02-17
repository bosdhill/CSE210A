load ../../harness

@test "d56289858a85" {
  check 'if (¬false)      then  

skip    else 
skip  ' '⇒ skip, {}'
}
