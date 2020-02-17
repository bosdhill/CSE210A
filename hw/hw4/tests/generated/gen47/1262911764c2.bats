load ../../harness

@test "1262911764c2" {
  check 'if (z+   -3    =x     *    2   ∨  v6 * -1    =Z     +v)  then z     :=  -4     *-3  else 
 z  :=     -3 -   x ' '⇒ z := (-4*-3), {}
⇒ skip, {z → 12}'
}
