load ../../harness

@test "5c7eb12ab640" {
  check 'z     :=     2   * 0 +    -1' '⇒ skip, {z → -1}'
}
