load ../../harness

@test "fae378abcc54" {
  check 'if (z  +  3   =    -2  +    3)      then  



y:= 1+    2      else  

 y  :=    -1 *     y    ' '⇒ y := (-1*y), {}
⇒ skip, {y → 0}'
}
