load ../../harness

@test "3151821337c8" {
  check 'if (¬false)    then  
 
skip   else skip  ' '⇒ skip, {}'
}
