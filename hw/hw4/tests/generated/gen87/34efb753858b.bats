load ../../harness

@test "34efb753858b" {
  check 'if (2    -  0=     F  *     y     ∧  z     +   3    <   -4  -    1)   then  im :=     -1     *-4 else 
z :=3 -  3 ' '⇒ z := (3-3), {}
⇒ skip, {z → 0}'
}
