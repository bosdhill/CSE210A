load ../../harness

@test "bce074996377" {
  check 'z :=  -4  -   z    ' '⇒ skip, {z → -4}'
}
