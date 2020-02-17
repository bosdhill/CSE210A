load ../../harness

@test "80984922494b" {
  check 'if (¬(3   *   -4   =   z  *    y))  then 

 
x   :=     -3    *    z      else  
  y   :=  0  *  -4     ' '⇒ x := (-3*z), {}
⇒ skip, {x → 0}'
}
