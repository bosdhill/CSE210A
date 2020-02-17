load ../../harness

@test "e82edb86fcc0" {
  check 'if (¬(y*    -2=    -1+    0)) then 
z   :=  -4   *    1  else skip ' '⇒ z := (-4*1), {}
⇒ skip, {z → -4}'
}
