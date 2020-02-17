load ../../harness

@test "0a5f88d19ba4" {
  check 'if (-4    *  -2   =z  +  p   ∧  3 - z     = 0    -    -2)      then   
 y   :=y+     z  else 
   z := -4     -     y     ' '⇒ z := (-4-y), {}
⇒ skip, {z → -4}'
}
