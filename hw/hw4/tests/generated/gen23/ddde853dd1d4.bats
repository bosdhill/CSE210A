load ../../harness

@test "ddde853dd1d4" {
  check 'if false   then   x     :=x+    -3  else    z := 1    ' '⇒ z := 1, {}
⇒ skip, {z → 1}'
}
