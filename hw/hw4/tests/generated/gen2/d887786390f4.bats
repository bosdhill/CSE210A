load ../../harness

@test "d887786390f4" {
  check 'z     :=x     * 4     ' '⇒ skip, {z → 0}'
}
