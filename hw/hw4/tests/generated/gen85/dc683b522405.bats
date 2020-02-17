load ../../harness

@test "dc683b522405" {
  check 'if (1     *  y <    y ∨    0*  e     =    x  *    1)     then 

z    := x     +  1     else   z   :=    1   *    -2   ' '⇒ z := (x+1), {}
⇒ skip, {z → 1}'
}
