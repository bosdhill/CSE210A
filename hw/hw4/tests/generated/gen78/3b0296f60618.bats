load ../../harness

@test "3b0296f60618" {
  check 'y     :=y    -    x ' '⇒ skip, {y → 0}'
}
