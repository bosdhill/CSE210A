load ../../harness

@test "3816901baa51" {
  check 'if (-2   *    0  <  1 -   4∨ -4     +     x<     -2     +   z)   then z     :=   -2      else 
 P:=    z   *   z  ' '⇒ z := -2, {}
⇒ skip, {z → -2}'
}
