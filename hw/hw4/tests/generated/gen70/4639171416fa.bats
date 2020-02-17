load ../../harness

@test "4639171416fa" {
  check 'z     :=  z    -4   ' '⇒ skip, {z → -4}'
}
