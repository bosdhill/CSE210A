load ../../harness

@test "b2b12505c5df" {
  check 'if (2     +  -3  <  x   *x     ∨true) then  
skip     else 
 skip   ' '⇒ skip, {}'
}
