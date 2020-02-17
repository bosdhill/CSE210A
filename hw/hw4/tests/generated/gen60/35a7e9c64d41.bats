load ../../harness

@test "35a7e9c64d41" {
  check 'if (4    +  x   <y-  -3   ∧   -2*   4  <x+    -2)     then skip   else   

 z  :=    y +     -1     ' '⇒ z := (y+-1), {}
⇒ skip, {z → -1}'
}
