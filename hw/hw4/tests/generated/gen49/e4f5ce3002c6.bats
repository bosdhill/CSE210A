load ../../harness

@test "e4f5ce3002c6" {
  check 'y    :=    z     -1 ' '⇒ skip, {y → -1}'
}
