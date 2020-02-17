load ../../harness

@test "ea046016f909" {
  check 'if (-1 - 2 <  x     + Bn)   then 

 x  :=  y     -     z  else 
  
 x :=   y   +   0   ' '⇒ x := (y-z), {}
⇒ skip, {x → 0}'
}
