load ../../harness

@test "741781e0c82e" {
  check 'y     := -2    *     z  ' '⇒ skip, {y → 0}'
}
