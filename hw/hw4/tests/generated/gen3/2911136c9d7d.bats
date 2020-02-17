load ../../harness

@test "2911136c9d7d" {
  check 'z  := -2 + -2 ' '⇒ skip, {z → -4}'
}
