load ../../harness

@test "ade62b53462f" {
  check 'if (¬false)      then  
   skip   else 
 x :=0     +     -3 ' '⇒ skip, {}'
}
