load ../../harness

@test "656810946f31" {
  check 'i   := z     +     y  ' '⇒ skip, {i → 0}'
}
