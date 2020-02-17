load ../../harness

@test "c2fbdaf8ff45" {
  check 'skip;y    :=    4*2   *    -1   ' '⇒ y := ((4*2)*-1), {}
⇒ skip, {y → -8}'
}
