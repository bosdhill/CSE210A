load ../../harness

@test "3bfccad72257" {
  check 'if (-3   *    1  <     z  *  z     ∧   4   -   y   <   1    *    4)   then z    := -1   +    z  else  



z    :=  x     +  0 ' '⇒ z := (x+0), {}
⇒ skip, {z → 0}'
}
