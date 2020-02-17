load ../../harness

@test "afa105e39e07" {
  check 'z   :=     y   * 2 ' '⇒ skip, {z → 0}'
}
