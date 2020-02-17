load ../../harness

@test "79de23095df2" {
  check 'if (false  ∨z <     x  + y) then   
 z    :=y  *x      else z :=     y -     g   ' '⇒ z := (y-g), {}
⇒ skip, {z → 0}'
}
