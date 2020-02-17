load ../../harness

@test "0b3ee14079ad" {
  check 'if (¬false)  then  
skip else 
 skip   ' '⇒ skip, {}'
}
