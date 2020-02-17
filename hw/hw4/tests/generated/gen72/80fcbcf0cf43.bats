load ../../harness

@test "80fcbcf0cf43" {
  check 'y   :=   x   *     z ' '⇒ skip, {y → 0}'
}
