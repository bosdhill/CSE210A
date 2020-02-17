load ../../harness

@test "f847bcf50f77" {
  check 'if (y   *  z <y-  0  ∧    x    *   y    = 0    +     y) then 


skip      else  z  :=     z     +  y    ' '⇒ z := (z+y), {}
⇒ skip, {z → 0}'
}
