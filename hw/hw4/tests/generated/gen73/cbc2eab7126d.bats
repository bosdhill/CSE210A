load ../../harness

@test "cbc2eab7126d" {
  check 'if (y    +-4   <    y   *     j    ∧-1    +-1    = 2     +   xw)     then   
 skip     else  y := z*     -3     ' '⇒ y := (z*-3), {}
⇒ skip, {y → 0}'
}
