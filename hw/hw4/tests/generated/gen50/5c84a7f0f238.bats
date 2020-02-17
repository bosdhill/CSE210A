load ../../harness

@test "5c84a7f0f238" {
  check 'z := z   *   x  ' '⇒ skip, {z → 0}'
}
