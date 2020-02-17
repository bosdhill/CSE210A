load ../../harness

@test "2c5984291393" {
  check 'if (¬(-2    -x <   z-    -1))     then   
x    :=    3    *   x   else  z := z    -x    ' '⇒ z := (z-x), {}
⇒ skip, {z → 0}'
}
