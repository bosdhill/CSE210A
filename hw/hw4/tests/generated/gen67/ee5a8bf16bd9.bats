load ../../harness

@test "ee5a8bf16bd9" {
  check 'x     := f+    z   ' '⇒ skip, {x → 0}'
}
