load ../../harness

@test "e860be901f62" {
  check 'z :=1 * x' '⇒ skip, {z → 0}'
}
