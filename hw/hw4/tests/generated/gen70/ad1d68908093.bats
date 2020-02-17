load ../../harness

@test "ad1d68908093" {
  check 'if (¬false) then 
skip else 
 

skip' '⇒ skip, {}'
}
