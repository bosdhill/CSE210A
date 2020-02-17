load ../../harness

@test "01ed186c1be3" {
  check 'if (¬(3   +   -3     <  1-     -3))   then z  :=  x      else  

 z :=  y *     z ' '⇒ z := (y*z), {}
⇒ skip, {z → 0}'
}
