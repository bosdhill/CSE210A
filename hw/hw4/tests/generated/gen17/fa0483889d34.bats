load ../../harness

@test "fa0483889d34" {
  check 'if (¬false)     then  
 skip    else 
  z:=-2  +    n     ' '⇒ skip, {}'
}
