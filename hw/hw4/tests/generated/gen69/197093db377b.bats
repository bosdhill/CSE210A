load ../../harness

@test "197093db377b" {
  check 'if (x   - z    <     y  *     4     ∨0 -  y<     z    -   -2)     then 

  z :=   -2     +aJ else  
 
skip' '⇒ z := (-2+aJ), {}
⇒ skip, {z → -2}'
}
