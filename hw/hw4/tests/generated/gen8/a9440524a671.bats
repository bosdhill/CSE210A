load ../../harness

@test "a9440524a671" {
  check 'z  :=     -1*   z    ' '⇒ skip, {z → 0}'
}
