load ../../harness

@test "eb45edaf80eb" {
  check 'if (x*   x     <   z -  z  ∧   false)   then  
 
 skip     else z:=     -2 ' '⇒ z := -2, {}
⇒ skip, {z → -2}'
}
