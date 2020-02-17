load ../../harness

@test "343478092170" {
  check 'z   :=    -2     - z   ' '⇒ skip, {z → -2}'
}
