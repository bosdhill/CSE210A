load ../../harness

@test "ff34edf8d050" {
  check 'z  :=   -1+    z    ' '⇒ skip, {z → -1}'
}
