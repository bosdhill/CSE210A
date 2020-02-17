load ../../harness

@test "9944018103b0" {
  check 'if (2--2   <     x *     z  ∨    -3  *   L < -1+     y)     then  y     :=   QQ   -    1     else 
 z     :=-1   +     -4    ' '⇒ z := (-1+-4), {}
⇒ skip, {z → -5}'
}
