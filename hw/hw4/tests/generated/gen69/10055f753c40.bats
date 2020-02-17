load ../../harness

@test "10055f753c40" {
  check 'z :=    0     * y    ' '⇒ skip, {z → 0}'
}
