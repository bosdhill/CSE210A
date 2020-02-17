load ../../harness

@test "acb24bf6c99c" {
  check 'if (¬(x   *  -2     <  y*x))   then 
 
x :=  0+    -2     else   
x:=    -4  *    z' '⇒ x := (0+-2), {}
⇒ skip, {x → -2}'
}
