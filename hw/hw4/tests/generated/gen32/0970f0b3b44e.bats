load ../../harness

@test "0970f0b3b44e" {
  check 'if (Y     * 4    =     0  +  4    ∨ x    +y    <  y-  -4)     then  
y :=-3+    4     else 
z  :=    z    ' '⇒ y := (-3+4), {}
⇒ skip, {y → 1}'
}
