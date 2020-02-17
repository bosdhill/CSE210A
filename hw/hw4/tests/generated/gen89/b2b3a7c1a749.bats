load ../../harness

@test "b2b3a7c1a749" {
  check 'if (y    +3=     z    + -4)    then   y  :=x+    0     else 
 y:=  2+     2 ' '⇒ y := (2+2), {}
⇒ skip, {y → 4}'
}
