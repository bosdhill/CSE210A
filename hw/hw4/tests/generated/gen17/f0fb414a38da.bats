load ../../harness

@test "f0fb414a38da" {
  check 'y     :=     z   *    y   ' '⇒ skip, {y → 0}'
}
