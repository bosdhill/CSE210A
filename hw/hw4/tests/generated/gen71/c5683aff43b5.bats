load ../../harness

@test "c5683aff43b5" {
  check 'z :=     -4     * 0 ' '⇒ skip, {z → 0}'
}
