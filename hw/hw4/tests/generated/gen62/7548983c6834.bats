load ../../harness

@test "7548983c6834" {
  check 'z  :=   z    ' '⇒ skip, {z → 0}'
}
