load ../../harness

@test "d3e475e5d349" {
  check 'if (-1     <y   -x     ∨     y   *  4   <     3   *     3)    then  
z  :=  y -  x      else 
 
skip     ' '⇒ z := (y-x), {}
⇒ skip, {z → 0}'
}
