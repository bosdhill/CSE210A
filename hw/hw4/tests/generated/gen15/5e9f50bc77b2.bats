load ../../harness

@test "5e9f50bc77b2" {
  check 'skip     ;z  :=     z  *   -2  ' '⇒ z := (z*-2), {}
⇒ skip, {z → 0}'
}
