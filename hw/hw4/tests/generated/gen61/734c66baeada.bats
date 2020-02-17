load ../../harness

@test "734c66baeada" {
  check 'y     :=    4   *    (-1 + -1)    ' '⇒ skip, {y → -8}'
}
