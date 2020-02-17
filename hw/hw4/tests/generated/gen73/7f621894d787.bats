load ../../harness

@test "7f621894d787" {
  check 'z    := y *    x ' '⇒ skip, {z → 0}'
}
