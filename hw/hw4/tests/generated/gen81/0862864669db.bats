load ../../harness

@test "0862864669db" {
  check 'z  :=     x * z ' '⇒ skip, {z → 0}'
}
