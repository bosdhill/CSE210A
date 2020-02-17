load ../../harness

@test "75516e073eaa" {
  check 'if (x   + x     <i  +-1  ∧   false)    then   d := U    *  z  else y :=     3   *y ' '⇒ y := (3*y), {}
⇒ skip, {y → 0}'
}
