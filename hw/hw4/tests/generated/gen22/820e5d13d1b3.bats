load ../../harness

@test "820e5d13d1b3" {
  check 'if (¬(-4 +   -2     <  z+ z))  then 
 x :=    y *     3      else    
 x:=  -4    +  Y9     ' '⇒ x := (-4+Y9), {}
⇒ skip, {x → -4}'
}
