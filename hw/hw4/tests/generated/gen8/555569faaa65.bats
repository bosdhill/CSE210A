load ../../harness

@test "555569faaa65" {
  check 'y   :=     1   * y  ' '⇒ skip, {y → 0}'
}
