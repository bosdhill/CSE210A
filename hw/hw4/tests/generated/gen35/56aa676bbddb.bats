load ../../harness

@test "56aa676bbddb" {
  check 'y    := 1    *    2    ' '⇒ skip, {y → 2}'
}
