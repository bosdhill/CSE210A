load ../../harness

@test "ee130dae647f" {
  check 'if (y *    1   = x  *    0   ∨  c8     *  4     <   4    *  -4)  then 
 x :=-1-0  else  
skip    ' '⇒ x := (-1-0), {}
⇒ skip, {x → -1}'
}
