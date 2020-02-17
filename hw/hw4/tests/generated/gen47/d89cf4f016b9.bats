load ../../harness

@test "d89cf4f016b9" {
  check 'if (-3    *    x   <  x ∧   x  *    z   = Pu* 0)      then   

skip  else z   := -2+     0     ' '⇒ z := (-2+0), {}
⇒ skip, {z → -2}'
}
