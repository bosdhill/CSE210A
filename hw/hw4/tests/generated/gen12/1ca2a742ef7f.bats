load ../../harness

@test "1ca2a742ef7f" {
  check 'if (y     +z     <  y *4 ∨    y   -   z =   0    *   z)   then 

z :=   V     *    x  else 
 z   :=   x    *x    ' '⇒ z := (V*x), {}
⇒ skip, {z → 0}'
}
