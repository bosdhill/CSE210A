load ../../harness

@test "617f737619cf" {
  check 'if (4    *    2=     x     +  P2    ∨  4    *     2 <  z+x)     then 
 st    :=   y  -   z      else y    := z  -2    ' '⇒ y := (z-2), {}
⇒ skip, {y → -2}'
}
