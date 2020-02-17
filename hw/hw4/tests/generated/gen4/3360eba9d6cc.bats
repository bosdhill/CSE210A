load ../../harness

@test "3360eba9d6cc" {
  check 'j8     :=    y   * -2   ' '⇒ skip, {j8 → 0}'
}
