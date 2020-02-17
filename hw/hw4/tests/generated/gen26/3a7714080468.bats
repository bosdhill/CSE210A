load ../../harness

@test "3a7714080468" {
  check 'y := z  *    -4    ' '⇒ skip, {y → 0}'
}
