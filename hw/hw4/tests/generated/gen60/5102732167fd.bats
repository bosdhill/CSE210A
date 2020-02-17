load ../../harness

@test "5102732167fd" {
  check 'z     :=    -1-  z ' '⇒ skip, {z → -1}'
}
