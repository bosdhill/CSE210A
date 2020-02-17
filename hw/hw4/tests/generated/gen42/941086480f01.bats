load ../../harness

@test "941086480f01" {
  check 'if (z-    z <   y     -   3   ∨false)    then  
skip else 
   skip' '⇒ skip, {}'
}
