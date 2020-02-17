load ../../harness

@test "00ee3292c6ef" {
  check 'y     :=   1*y   ' '⇒ skip, {y → 0}'
}
