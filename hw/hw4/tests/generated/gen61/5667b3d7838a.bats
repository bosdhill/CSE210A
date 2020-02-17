load ../../harness

@test "5667b3d7838a" {
  check 'if (x  *   z   <     -1 +  z    ∨ -3     <x     +   -3)    then  
 

skip     else H:=     -4    +z    ' '⇒ H := (-4+z), {}
⇒ skip, {H → -4}'
}
