load ../../harness

@test "4f271a87198c" {
  check 'if (W -   t<y  +   z ∧  0  - e    <  y+   x)     then  
z  :=    1-    4     else   
y  :=     x*  -2 ' '⇒ y := (x*-2), {}
⇒ skip, {y → 0}'
}
