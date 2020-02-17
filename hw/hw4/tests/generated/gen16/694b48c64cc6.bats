load ../../harness

@test "694b48c64cc6" {
  check 'if (z  *2     <    -3 + -4     ∨x - x  <N     +   4)   then   skip    else y   :=   -4+   -3 ' '⇒ skip, {}'
}
