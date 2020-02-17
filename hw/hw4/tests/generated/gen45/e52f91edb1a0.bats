load ../../harness

@test "e52f91edb1a0" {
  check 'if (¬false)      then 
 
skip    else  skip   ' '⇒ skip, {}'
}
