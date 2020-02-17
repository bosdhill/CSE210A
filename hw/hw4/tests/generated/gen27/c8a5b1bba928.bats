load ../../harness

@test "c8a5b1bba928" {
  check 'if (¬(-4   -   x   =   4    +    1)) then   
z     :=   3 *   vx   else   skip    ' '⇒ z := (3*vx), {}
⇒ skip, {z → 0}'
}
