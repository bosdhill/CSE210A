load ../../harness

@test "2f1d59819c47" {
  check 'y     := 1     *  y' '⇒ skip, {y → 0}'
}
