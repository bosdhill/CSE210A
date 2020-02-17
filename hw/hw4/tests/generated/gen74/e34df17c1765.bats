load ../../harness

@test "e34df17c1765" {
  check 'if (x   =   2     *   z     ∧-3+    -4 <  -3 *  S)     then     
z:=     -1    -2 else   K   :=     y     *  3 ' '⇒ z := (-1-2), {}
⇒ skip, {z → -3}'
}
