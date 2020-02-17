load ../../harness

@test "85026a86a98c" {
  check 'z :=    z   * -2  ' '⇒ skip, {z → 0}'
}
