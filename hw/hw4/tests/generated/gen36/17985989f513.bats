load ../../harness

@test "17985989f513" {
  check 'y     :=     -3     +y  ' '⇒ skip, {y → -3}'
}
