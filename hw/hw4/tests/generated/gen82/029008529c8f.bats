load ../../harness

@test "029008529c8f" {
  check 'if (-4   <   -3-  z)   then 

  z  :=     x +     0   else z :=     x+     x   ' '⇒ z := (x+0), {}
⇒ skip, {z → 0}'
}
