load ../../harness

@test "52869ece0516" {
  check 'y  :=    -4     + Z   ' '⇒ skip, {y → -4}'
}
